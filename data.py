import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

def force_directed_label_placement(data_file, iterations=100, repulsion_strength=100, attraction_strength=10, damping=0.9, min_distance=5):
    """
    Performs force-directed label placement for CAD-like data.

    Args:
        data_file (str): Path to the CSV file containing label data.
        iterations (int): Number of iterations for the algorithm.
        repulsion_strength (float): Strength of the repulsive force between labels.
        attraction_strength (float): Strength of the attractive force to the original data point.
        damping (float): Damping factor to reduce oscillations.
        min_distance (float): Minimum distance between labels to avoid excessive repulsion.

    Returns:
        pandas.DataFrame: DataFrame with updated label positions.
    """

    df = pd.read_csv(data_file)
    positions = df[['Position X', 'Position Y']].values.astype(float)  # Use 'Position X' and 'Position Y'

    for _ in range(iterations):
        forces = np.zeros_like(positions)

        # Repulsion force
        for i in range(len(positions)):
            for j in range(i + 1, len(positions)):
                dist = np.linalg.norm(positions[i] - positions[j])
                if dist < min_distance:
                    dist = min_distance
                direction = positions[i] - positions[j]
                force = repulsion_strength / (dist ** 2) * direction / dist
                forces[i] += force
                forces[j] -= force

        # Attraction force
        original_positions = df[['Position X', 'Position Y']].values.astype(float) # Use 'Position X' and 'Position Y'
        forces += attraction_strength * (original_positions - positions)

        # Update positions with damping
        positions += forces * damping

    df[['Position X', 'Position Y']] = positions # Update the original columns
    return df

def visualize_labels(df, original_data=True):
    """
    Visualizes the original and updated label positions.

    Args:
        df (pandas.DataFrame): DataFrame containing label data.
        original_data (bool): if true, plots the original data points
    """
    plt.figure(figsize=(10, 8))
    if original_data:
        plt.scatter(df['Position X_original'], df['Position Y_original'], marker='.', color='gray', label='Original Data') # Use 'Position X_original' and 'Position Y_original'
    plt.scatter(df['Position X'], df['Position Y'], marker='o', color='red', label='Label Positions')

    for index, row in df.iterrows():
        plt.annotate(row['Contents'], (row['Position X'], row['Position Y'])) # Use 'Contents'

    plt.xlabel('Position X')
    plt.ylabel('Position Y')
    plt.title('Force-Directed Label Placement')
    plt.legend()
    plt.grid(True)
    plt.show()

# Example usage:
# Assuming your CSV file is named 'your_data.csv'
df = pd.read_csv('ACAD-FILE 1.csv') # Replace with your actual file name
df['Position X_original'] = df['Position X'] #store original values for plotting
df['Position Y_original'] = df['Position Y']
df_placed = force_directed_label_placement('ACAD-FILE 1.csv')

# Visualize the results
# visualize_labels(df_placed)s