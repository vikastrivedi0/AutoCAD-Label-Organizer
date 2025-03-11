import math
from typing import List, Tuple, Dict
from dataclasses import dataclass
from pyautocad import Autocad, APoint
import win32com.client
import pythoncom

@dataclass
class Point:
    x: float
    y: float
    z: float = 0.0

    def __str__(self):
        return f"({self.x:.2f}, {self.y:.2f}, {self.z:.2f})"

    @classmethod
    def from_apoint(cls, apoint):
        """Create a Point from an AutoCAD APoint."""
        return cls(apoint[0], apoint[1], apoint[2] if len(apoint) > 2 else 0.0)

@dataclass
class Label:
    id: str
    corners: List[Point]
    insertion_point: Point
    label_type: str  # Added to store label type (PIPE or STRUCTURE)

    def __str__(self):
        return f"Label {self.id} ({self.label_type}):\n" + \
               "\n".join(f"  Corner {i+1}: {pt}" for i, pt in enumerate(self.corners))

class AutoCADInterface:
    def __init__(self):
        """Initialize connection to AutoCAD."""
        try:
            self.acad = Autocad()
            print(f"Connected to AutoCAD version: {self.acad.doc.Application.Version}")
        except Exception as e:
            print(f"Error connecting to AutoCAD: {e}")
            raise

    def get_label_corners(self, entity) -> Tuple[List[Point], Point]:
        """Get corners and insertion point of a label entity."""
        try:
            # Get the bounding box
            bbox = entity.GetBoundingBox()
            if bbox is None:
                return None, None

            min_point, max_point = bbox
            
            # Convert to our Point class
            min_pt = Point.from_apoint(min_point)
            max_pt = Point.from_apoint(max_point)
            
            # Create all four corners
            corners = [
                Point(min_pt.x, min_pt.y, min_pt.z),  # Bottom-left
                Point(max_pt.x, min_pt.y, min_pt.z),  # Bottom-right
                Point(max_pt.x, max_pt.y, min_pt.z),  # Top-right
                Point(min_pt.x, max_pt.y, min_pt.z)   # Top-left
            ]
            
            # Get insertion point
            insertion_point = Point.from_apoint(entity.InsertionPoint)
            
            return corners, insertion_point
        except Exception as e:
            print(f"Error getting corners for entity: {e}")
            return None, None

    def get_all_labels(self) -> List[Tuple[str, List[Point], Point, str]]:
        """Get all Civil 3D labels from the drawing."""
        labels_data = []
        
        try:
            # Get all entities in model space
            for entity in self.acad.iter_objects():
                label_type = None
                
                # Check if entity is a Civil 3D label
                if hasattr(entity, 'ObjectName'):
                    if 'AECC_PIPE_LABEL' in entity.ObjectName:
                        label_type = 'PIPE'
                    elif 'AECC_STRUCTURE_LABEL' in entity.ObjectName:
                        label_type = 'STRUCTURE'
                
                if label_type:
                    corners, insertion_point = self.get_label_corners(entity)
                    if corners and insertion_point:
                        label_id = str(entity.Handle)  # Use entity handle as unique ID
                        labels_data.append((label_id, corners, insertion_point, label_type))
                        print(f"Found {label_type} label with handle: {label_id}")
        
        except Exception as e:
            print(f"Error getting labels: {e}")
        
        return labels_data

class LabelOverlapDetector:
    def __init__(self):
        self.labels: Dict[str, Label] = {}

    def add_label(self, label_id: str, corners: List[Point], 
                 insertion_point: Point, label_type: str = "UNKNOWN"):
        """Add a label with its corners and insertion point."""
        label = Label(
            id=label_id,
            corners=corners,
            insertion_point=insertion_point,
            label_type=label_type
        )
        self.labels[label_id] = label
        print(f"\nAdded {label}")

    def lines_intersect(self, p1: Point, p2: Point, p3: Point, p4: Point) -> bool:
        """Check if line segments (p1,p2) and (p3,p4) intersect."""
        def ccw(A: Point, B: Point, C: Point) -> bool:
            return (C.y - A.y) * (B.x - A.x) > (B.y - A.y) * (C.x - A.x)

        return ccw(p1, p3, p4) != ccw(p2, p3, p4) and ccw(p1, p2, p3) != ccw(p1, p2, p4)

    def point_in_polygon(self, point: Point, polygon: List[Point]) -> bool:
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

    def polygons_overlap(self, corners1: List[Point], corners2: List[Point]) -> bool:
        """Check if two polygons overlap."""
        # Check if any line segments intersect
        n1, n2 = len(corners1), len(corners2)
        for i in range(n1):
            for j in range(n2):
                if self.lines_intersect(
                    corners1[i], corners1[(i + 1) % n1],
                    corners2[j], corners2[(j + 1) % n2]
                ):
                    return True

        # Check if one polygon is completely inside the other
        return (self.point_in_polygon(corners1[0], corners2) or
                self.point_in_polygon(corners2[0], corners1))

    def find_overlapping_labels(self) -> List[Tuple[Label, Label]]:
        """Find all pairs of overlapping labels."""
        overlapping_pairs = []
        label_ids = list(self.labels.keys())
        
        for i in range(len(label_ids)):
            for j in range(i + 1, len(label_ids)):
                label1 = self.labels[label_ids[i]]
                label2 = self.labels[label_ids[j]]
                
                if self.polygons_overlap(label1.corners, label2.corners):
                    overlapping_pairs.append((label1, label2))
                    print(f"\nFound overlap between:")
                    print(f"Label 1:\n{label1}")
                    print(f"Label 2:\n{label2}")
                    
        return overlapping_pairs

def process_dwg_file():
    """Process the currently open DWG file in AutoCAD."""
    try:
        # Initialize AutoCAD interface
        print("\nConnecting to AutoCAD...")
        acad_interface = AutoCADInterface()
        
        # Initialize overlap detector
        detector = LabelOverlapDetector()
        
        # Get all labels from the drawing
        print("\nReading labels from drawing...")
        labels_data = acad_interface.get_all_labels()
        
        # Add labels to detector
        print(f"\nProcessing {len(labels_data)} labels...")
        for label_id, corners, insertion_point, label_type in labels_data:
            detector.add_label(label_id, corners, insertion_point, label_type)
        
        # Find overlapping labels
        print("\nChecking for overlaps...")
        overlapping_pairs = detector.find_overlapping_labels()
        
        if not overlapping_pairs:
            print("\nNo overlapping labels found.")
        else:
            print(f"\nFound {len(overlapping_pairs)} overlapping label pairs.")
            
            # Detailed overlap report
            print("\nDetailed Overlap Report:")
            print("=" * 50)
            for label1, label2 in overlapping_pairs:
                print(f"\nOverlap between {label1.label_type} label {label1.id}")
                print(f"and {label2.label_type} label {label2.id}")
                print(f"Label 1 insertion point: {label1.insertion_point}")
                print(f"Label 2 insertion point: {label2.insertion_point}")
                print("-" * 30)
        
    except Exception as e:
        print(f"Error processing DWG file: {e}")

if __name__ == "__main__":
    # If running as script, process the currently open DWG file
    process_dwg_file() 