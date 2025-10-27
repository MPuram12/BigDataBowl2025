import pandas as pd
import numpy as np

def compute_output_features(input_df, output_df, supplementary_df):
    """
    Processes the output dataframe to add new features based on input data.

    This function:
    1. Gets the unique ball landing spot (x, y) for each play from the input_df.
    2. Merges this landing spot info into the output_df.
    3. Merges the 'pass_result' from the supplementary_df.
    4. Gets and merges static player data (position, role, side) from input_df.
    5. Calculates 'time_left_s': (max_frame - current_frame) / 10.
    6. Calculates 'dist_to_ball_land': Euclidean distance from player (x, y)
       to the (ball_land_x, ball_land_y).
    7. Calculates 'speed', 'accel' (tangential), 'direction' (of motion),
       and 'accel_direction' (direction of total acceleration vector)
       by linking the last input frame to the first output frame.

    Args:
        input_df (pd.DataFrame): The complete dataframe from an 'input_*.csv' file.
        output_df (pd.DataFrame): The complete dataframe from an 'output_*.csv' file.
        supplementary_df (pd.DataFrame): The dataframe from 'supplementary_data.csv'.

    Returns:
        pd.DataFrame: A copy of the output_df with the new
                      'time_left_s' and 'dist_to_ball_land' columns added.
    """
    
    # --- 1. Get unique ball landing spots from input data ---
    # We assume 'ball_land_x' and 'ball_land_y' are constant for a given play.
    # We MUST use drop_duplicates(subset=...) to ensure one row per play,
    # preventing a cartesian product (memory error) during the merge.
    play_landing_spots = input_df[
        ['game_id', 'play_id', 'ball_land_x', 'ball_land_y']
    ].drop_duplicates(subset=['game_id', 'play_id'])

    # --- 2. Merge landing spots into output data ---
    # This adds 'ball_land_x' and 'ball_land_y' to every row in output_df
    data = pd.merge(
        output_df, 
        play_landing_spots, 
        on=['game_id', 'play_id'], 
        how='left'
    )
    
    # Handle cases where a play in output might not have ball_land data
    if data['ball_land_x'].isnull().any():
        print("Warning: Some output plays had no matching ball_land data in input.")
        # Fill with 0 or np.nan if you prefer to handle it differently
        data['ball_land_x'] = data['ball_land_x'].fillna(0)
        data['ball_land_y'] = data['ball_land_y'].fillna(0)

    # --- 3. Merge pass_result from supplementary data ---
    cols_to_merge = ['pass_result', 'route_of_targeted_receiver']
    
    # Find which of these columns actually exist in the supplementary_df
    existing_cols_to_merge = [col for col in cols_to_merge if col in supplementary_df.columns]
    
    if existing_cols_to_merge:
        # Create a list of keys + columns to extract
        lookup_cols = ['game_id', 'play_id'] + existing_cols_to_merge
        
        # Extract and de-duplicate based on the play
        play_supplementary_data = supplementary_df[lookup_cols].drop_duplicates(
            subset=['game_id', 'play_id']
        )
        
        # Merge this data in
        data = pd.merge(
            data,
            play_supplementary_data,
            on=['game_id', 'play_id'],
            how='left'
        )
    else:
        print("Warning: No supplementary columns ('pass_result', 'route_of_targeted_receiver') found.")

    # Ensure all requested columns exist, even if they weren't in the file
    # This prevents errors if they are expected later.
    for col in cols_to_merge:
        if col not in data.columns:
            print(f"Warning: '{col}' not found in supplementary data. Adding as empty column.")
            data[col] = np.nan

    # --- 4. Get and merge static player data ---
    # Get unique player info (position, role, side) from the input data
    # Use drop_duplicates(subset=...) to ensure one row per player-play
    player_static_data = input_df[
        ['game_id', 'play_id', 'nfl_id', 'player_position', 'player_role', 'player_side']
    ].drop_duplicates(subset=['game_id', 'play_id', 'nfl_id'])

    data = pd.merge(
        data,
        player_static_data,
        on=['game_id', 'play_id', 'nfl_id'],
        how='left'
    )

    # --- 5. Calculate max frame for each play ---
    # We use transform('max') to create a new column where every row
    # for a play contains the max 'frame_id' for that play.
    data['max_frame'] = data.groupby(
        ['game_id', 'play_id']
    )['frame_id'].transform('max')

    # --- 6. Calculate 'time_left_s' ---
    # (max_frame - current_frame) / 10 (assuming 10 frames per second)
    data['time_left_s'] = (data['max_frame'] - data['frame_id']) / 10.0

    # --- 7. Calculate 'dist_to_ball_land' ---
    # Euclidean distance: sqrt((x2-x1)^2 + (y2-y1)^2)
    data['dist_to_ball_land'] = np.sqrt(
        (data['x'] - data['ball_land_x'])**2 + 
        (data['y'] - data['ball_land_y'])**2
    )
    # --- 8. Get last input frame (x,y,s,dir) for velocity/accel calculation ---
    # We need the last known position (x,y), speed (s), and direction (dir)
    # from the input data to calculate the first frame of the output data.
    
    # Find the index of the max frame_id for each player/play
    try:
        # This groupby/idxmax operation is inherently unique and correct.
        # No cartesian product can happen from this merge.
        idx = input_df.groupby(
            ['game_id', 'play_id', 'nfl_id']
        )['frame_id'].idxmax()
        
        # Get the x, y, s, and dir data at that last frame
        last_input_frames = input_df.loc[idx, [
            'game_id', 'play_id', 'nfl_id', 'x', 'y', 's', 'dir'
        ]].rename(columns={
            'x': 'last_input_x', 
            'y': 'last_input_y',
            's': 'last_input_speed',
            'dir': 'last_input_dir'
        })

        # Merge this last (x,y,s,dir) into our output data
        data = pd.merge(
            data, 
            last_input_frames, 
            on=['game_id', 'play_id', 'nfl_id'], 
            how='left'
        )
    except KeyError:
        # This can happen if input_df is missing 'frame_id', 'x', 'y', 's', or 'dir'
        print("Warning: Could not find 'frame_id','x','y','s', or 'dir' in input_df.")
        data['last_input_x'] = np.nan
        data['last_input_y'] = np.nan
        data['last_input_speed'] = np.nan
        data['last_input_dir'] = np.nan

    # --- 9. Calculate velocities and angles ---
    # Sort values to ensure 'shift' operations work correctly within each group
    data = data.sort_values(by=['game_id', 'play_id', 'nfl_id', 'frame_id'])
    
    # Group by player
    grouped = data.groupby(['game_id', 'play_id', 'nfl_id'])
    
    # Get the lagged x and y (i.e., position from the previous frame)
    data['lag_x'] = grouped['x'].shift(1)
    data['lag_y'] = grouped['y'].shift(1)
    
    # Fill the *first* frame's lag with the *last input frame's* position
    data['lag_x'] = np.where(
        data['lag_x'].isnull(), 
        data['last_input_x'], 
        data['lag_x']
    )
    data['lag_y'] = np.where(
        data['lag_y'].isnull(), 
        data['last_input_y'], 
        data['lag_y']
    )
    
    # Now calculate derivatives
    data['dx'] = data['x'] - data['lag_x']
    data['dy'] = data['y'] - data['lag_y']
    data['dt'] = 0.1  # 10 frames per second
    
    # --- Motion (Velocity) Calculations ---
    data['speed'] = np.sqrt(data['dx']**2 + data['dy']**2) / data['dt']
    # Get direction in degrees
    data['direction'] = np.arctan2(data['dy'], data['dx']) * 180 / np.pi
    # Get velocity components
    data['v_x'] = data['dx'] / data['dt']
    data['v_y'] = data['dy'] / data['dt']


    # --- Acceleration Calculations ---
    
    # Get lag_speed to calculate tangential acceleration
    data['lag_speed'] = grouped['speed'].shift(1)
    
    # Fill the *first* frame's lag_speed with the *last input frame's* speed ('s')
    data['lag_speed'] = np.where(
        data['lag_speed'].isnull(), 
        data['last_input_speed'], 
        data['lag_speed']
    )
    
    # This is tangential acceleration (change in speed magnitude)
    data['accel'] = (data['speed'] - data['lag_speed']) / data['dt']
    
    # --- Acceleration Direction (from vector change) ---
    
    # Get lagged velocity components
    data['lag_v_x'] = grouped['v_x'].shift(1)
    data['lag_v_y'] = grouped['v_y'].shift(1)
    
    # Calculate v_x and v_y for the last input frame
    data['last_input_dir_rad'] = data['last_input_dir'] * (np.pi / 180)
    data['last_input_v_x'] = data['last_input_speed'] * np.cos(data['last_input_dir_rad'])
    data['last_input_v_y'] = data['last_input_speed'] * np.sin(data['last_input_dir_rad'])

    # Fill the *first* frame's lag_v_x/y with the *last input frame's* v_x/y
    data['lag_v_x'] = np.where(
        data['lag_v_x'].isnull(), 
        data['last_input_v_x'], 
        data['lag_v_x']
    )
    data['lag_v_y'] = np.where(
        data['lag_v_y'].isnull(), 
        data['last_input_v_y'], 
        data['lag_v_y']
    )

    # Calculate change in velocity components
    data['dv_x'] = data['v_x'] - data['lag_v_x']
    data['dv_y'] = data['v_y'] - data['lag_v_y']

    # Get direction of the acceleration vector
    data['accel_direction'] = np.arctan2(data['dv_y'], data['dv_x']) * 180 / np.pi


    # Calculate the optimal angle to the ball landing spot
    data['optimal_angle'] = np.arctan2(data['ball_land_y']-data['y'], data['ball_land_x']-data['x']) * 180 /np.pi
    
    # Calculate the raw difference
    raw_angle_diff = data['direction'] - data['optimal_angle']
    
    # Wrap the angle difference to the range [-180, 180] to find the smallest angle
    # This formula correctly handles the "wrap-around" (e.g., 10 deg vs 350 deg)
    # (diff + 180) % 360 - 180
    wrapped_angle_diff = (raw_angle_diff + 180) % 360 - 180
    
    # The final angle_diff is the absolute value of this wrapped difference
    data['angle_diff'] = abs(wrapped_angle_diff)    
    
    # --- 10. Clean up and return ---
    # Drop all the temporary helper columns
    final_data = data.drop(columns=[
        'max_frame', 'last_input_x', 'last_input_y', 'last_input_speed', 
        'last_input_dir', 'last_input_dir_rad', 'last_input_v_x', 'last_input_v_y',
        'lag_x', 'lag_y', 'dx', 'dy', 'dt', 'lag_speed',
        'v_x', 'v_y', 'lag_v_x', 'lag_v_y', 'dv_x', 'dv_y'
    ])
    
    return final_data

def generate_clean_data(week_num):
    """
    Loads the input, output, and supplementary data, then
    computes the clean output features.

    Args:
        week_num (int): The week number (1 through 18).

    Returns:
        pd.DataFrame: Processed output data with new features.
    """
    # Format week number as a two-digit string (e.g., 1 -> "01")
    week_str = str(week_num).zfill(2)
    
    # Define paths
    # Use forward slashes for cross-platform path compatibility
    input_path = f'raw_data/input_2023_w{week_str}.csv'
    output_path = f'raw_data/output_2023_w{week_str}.csv'
    supplementary_path = 'supplementary_data.csv'

    print(f"Loading data for week {week_num}...")
    print(f"Input file: {input_path}")
    print(f"Output file: {output_path}")
    print(f"Supplementary file: {supplementary_path}")

    # Load main weekly files
    raw_data_input = pd.read_csv(input_path)
    raw_data_output = pd.read_csv(output_path)
    
    # Load supplementary data
    try:
        supplementary_data = pd.read_csv(supplementary_path)
    except FileNotFoundError:
        print(f"Warning: {supplementary_path} not found. 'pass_result' will be missing.")
        # Create empty df with expected columns to prevent merge errors
        supplementary_data = pd.DataFrame(columns=['game_id', 'play_id', 'pass_result'])

    # Call the processing function to get the final data
    return compute_output_features(raw_data_input, raw_data_output, supplementary_data)

