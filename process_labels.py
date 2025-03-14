import pandas as pd
import numpy as np
from dataclasses import dataclass
from typing import List, Tuple
import matplotlib.pyplot as plt

@dataclass
class Point:
    x: float
    y: float
    z: float = 0.0

    def __str__(self):
        return f"({self.x:.2f}, {self.y:.2f}, {self.z:.2f})"

@dataclass
class Label:
    id: str
    label_type: str
    corners: List[Point]
    insertion_point: Point

    def plot(self, ax, color='blue', alpha=0.3):
        """Plot the label on a matplotlib axis."""
        # Extract x and y coordinates
        x = [p.x for p in self.corners + [self.corners[0]]]  # Add first point to close polygon
        y = [p.y for p in self.corners + [self.corners[0]]]
        
        # Plot polygon
        ax.fill(x, y, alpha=alpha, color=color)
        # Plot insertion point
        ax.plot(self.insertion_point.x, self.insertion_point.y, 'k.')
        # Add label ID
        ax.text(self.insertion_point.x, self.insertion_point.y, self.id, 
                fontsize=8, ha='right', va='bottom')

def load_labels(csv_file: str) -> List[Label]:
    """Load label data from CSV file."""
    df = pd.read_csv(csv_file)
    labels = []
    
    for _, row in df.iterrows():
        # Create corner points
        corners = [
            Point(row.bl_x, row.bl_y, row.bl_z),  # Bottom-left
            Point(row.br_x, row.br_y, row.br_z),  # Bottom-right
            Point(row.tr_x, row.tr_y, row.tr_z),  # Top-right
            Point(row.tl_x, row.tl_y, row.tl_z)   # Top-left
        ]
        
        # Create insertion point
        insertion_point = Point(row.ins_x, row.ins_y, row.ins_z)
        
        # Create label
        label = Label(
            id=row.id,
            label_type=row.type,
            corners=corners,
            insertion_point=insertion_point
        )
        labels.append(label)
    
    print(f"Loaded {len(labels)} labels from {csv_file}")
    return labels

def lines_intersect(p1: Point, p2: Point, p3: Point, p4: Point) -> bool:
    """Check if line segments (p1,p2) and (p3,p4) intersect."""
    def ccw(A: Point, B: Point, C: Point) -> bool:
        return (C.y - A.y) * (B.x - A.x) > (B.y - A.y) * (C.x - A.x)

    return ccw(p1, p3, p4) != ccw(p2, p3, p4) and ccw(p1, p2, p3) != ccw(p1, p2, p4)

def point_in_polygon(point: Point, polygon: List[Point]) -> bool:
    """Check if a point is inside a polygon using ray casting algorithm."""
    inside = False
    j = len(polygon) - 1
    
    for i in range(len(polygon)):
        if ((polygon[i].y > point.y) != (polygon[j].y > point.y) and
            point.x < (polygon[j].x - polygon[i].x) * (point.y - polygon[i].y) /
                     (polygon[j].y - polygon[i].y) + polygon[i].x):
            inside = not inside
        j = i
        
    return inside

def polygons_overlap(corners1: List[Point], corners2: List[Point]) -> bool:
    """Check if two polygons overlap."""
    # Check if any line segments intersect
    n1, n2 = len(corners1), len(corners2)
    for i in range(n1):
        for j in range(n2):
            if lines_intersect(
                corners1[i], corners1[(i + 1) % n1],
                corners2[j], corners2[(j + 1) % n2]
            ):
                return True

    # Check if one polygon is completely inside the other
    return (point_in_polygon(corners1[0], corners2) or
            point_in_polygon(corners2[0], corners1))

def find_overlapping_labels(labels: List[Label]) -> List[Tuple[Label, Label]]:
    """Find all pairs of overlapping labels."""
    overlapping_pairs = []
    
    for i in range(len(labels)):
        for j in range(i + 1, len(labels)):
            if polygons_overlap(labels[i].corners, labels[j].corners):
                overlapping_pairs.append((labels[i], labels[j]))
                print(f"\nFound overlap between:")
                print(f"Label 1: {labels[i].id} ({labels[i].label_type})")
                print(f"Label 2: {labels[j].id} ({labels[j].label_type})")
    
    return overlapping_pairs

def visualize_labels(labels: List[Label], overlapping_pairs: List[Tuple[Label, Label]]):
    """Create a visualization of all labels and highlight overlapping ones."""
    fig, ax = plt.subplots(figsize=(12, 8))
    
    # Plot all labels in light blue
    for label in labels:
        label.plot(ax, color='lightblue', alpha=0.2)
    
    # Plot overlapping labels in red and green
    overlapping_ids = set()
    for label1, label2 in overlapping_pairs:
        label1.plot(ax, color='red', alpha=0.3)
        label2.plot(ax, color='green', alpha=0.3)
        overlapping_ids.add(label1.id)
        overlapping_ids.add(label2.id)
    
    # Set axis labels and title
    ax.set_xlabel('X Coordinate')
    ax.set_ylabel('Y Coordinate')
    ax.set_title('Label Positions (Red/Green = Overlapping)')
    
    # Add legend
    from matplotlib.patches import Patch
    legend_elements = [
        Patch(facecolor='lightblue', alpha=0.2, label='Non-overlapping'),
        Patch(facecolor='red', alpha=0.3, label='Overlapping (1)'),
        Patch(facecolor='green', alpha=0.3, label='Overlapping (2)')
    ]
    ax.legend(handles=legend_elements)
    
    # Equal aspect ratio
    ax.set_aspect('equal')
    
    plt.show()

def main():
    # Load labels from CSV
    labels = load_labels('label_data.csv')
    
    # Find overlapping labels
    overlapping_pairs = find_overlapping_labels(labels)
    
    # Print summary
    print(f"\nFound {len(overlapping_pairs)} overlapping pairs")
    
    # Create visualization
    visualize_labels(labels, overlapping_pairs)

if __name__ == "__main__":
    main() 