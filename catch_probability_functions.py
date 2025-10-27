import pandas as pd
import os

def get_last_frame_data(df):
    """
    Filters a dataframe to include only the rows 
    where 'time_left_s' is equal to 0.

    Args:
      df (pd.DataFrame): The dataframe to filter, which must 
                         contain the 'time_left_s' column.

    Returns:
      pd.DataFrame: A new dataframe containing only the last frame
                    of each play (where time_left_s == 0).
    """
    if 'time_left_s' not in df.columns:
        print("Error: 'time_left_s' column not found in dataframe.")
        # Return an empty dataframe with the same columns
        return df.iloc[0:0] 

    # Filter the dataframe where time_left_s is 0
    # .copy() is used to avoid a SettingWithCopyWarning
    last_frame_df = df[df['time_left_s'] == 0].copy()
    
    return last_frame_df


def load_all_last_frames(input_dir='clean_data'):
    """
    Iterates through 'week1.csv' to 'week18.csv' in the input_dir,
    loads each file, filters for the last frame using get_last_frame_data,
    and concatenates them into a single dataframe.

    Args:
      input_dir (str): The directory where the 'week{i}.csv' files are stored.

    Returns:
      pd.DataFrame: A single dataframe containing the last frame of all plays
                    from all 18 weeks.
    """
    print(f"Starting to load data from directory: {input_dir}")
    all_last_frames_list = []

    # Iterate from week 1 to 18
    for week in range(1, 19):
        file_path = os.path.join(input_dir, f'week{week}.csv')
        
        try:
            # Load the clean data file for the week
            print(f"Loading {file_path}...")
            week_df = pd.read_csv(file_path)
            
            # Use the helper function to filter for the last frame
            last_frames_df = get_last_frame_data(week_df)
            
            # Add the filtered data to our list
            all_last_frames_list.append(last_frames_df)
            print(f"Found {len(last_frames_df)} plays in week {week}.")
            
        except FileNotFoundError:
            print(f"Warning: {file_path} not found. Skipping week {week}.")
        except pd.errors.EmptyDataError:
            print(f"Warning: {file_path} is empty. Skipping week {week}.")
        except Exception as e:
            print(f"An error occurred with {file_path}: {e}. Skipping.")

    # Check if we actually loaded any data
    if not all_last_frames_list:
        print(f"Error: No data loaded. Is the '{input_dir}' directory empty or in the wrong location?")
        return pd.DataFrame() # Return an empty dataframe

    # Concatenate all the individual dataframes into one big one
    print("\nConcatenating all weeks into one dataframe...")
    final_df = pd.concat(all_last_frames_list, ignore_index=True)
    
    print(f"Successfully combined last frames from {len(all_last_frames_list)} weeks.")
    return final_df