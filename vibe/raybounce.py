import math
import sys

class Vec2:
    """2D vector class for point and direction operations."""
    def __init__(self, x, y):
        self.x = x
        self.y = y
    
    def __add__(self, other):
        return Vec2(self.x + other.x, self.y + other.y)
    
    def __sub__(self, other):
        return Vec2(self.x - other.x, self.y - other.y)
    
    def __mul__(self, scalar):
        return Vec2(self.x * scalar, self.y * scalar)
    
    def dot(self, other):
        return self.x * other.x + self.y * other.y
    
    def length(self):
        return math.sqrt(self.x * self.x + self.y * self.y)
    
    def normalize(self):
        l = self.length()
        if l > 0:
            return Vec2(self.x / l, self.y / l)
        return Vec2(0, 0)
    
    def __repr__(self):
        return f"Vec2({self.x}, {self.y})"


class Box:
    """Represents a rotated rectangular box."""
    def __init__(self, center_x, center_y, width, height, rotation_deg, color):
        self.center = Vec2(center_x, center_y)
        self.width = width
        self.height = height
        self.rotation = math.radians(rotation_deg)
        self.color = color
        
        # Compute the four corners of the rotated box
        self.corners = self._compute_corners()
        # Compute the four edges as (start_point, end_point) pairs
        self.edges = [
            (self.corners[0], self.corners[1]),
            (self.corners[1], self.corners[2]),
            (self.corners[2], self.corners[3]),
            (self.corners[3], self.corners[0])
        ]
    
    def _compute_corners(self):
        """Compute the four corners of the rotated rectangle."""
        cos_r = math.cos(self.rotation)
        sin_r = math.sin(self.rotation)
        
        # Half dimensions
        hw = self.width / 2
        hh = self.height / 2
        
        # Local corners (before rotation)
        local_corners = [
            Vec2(-hw, -hh),
            Vec2(hw, -hh),
            Vec2(hw, hh),
            Vec2(-hw, hh)
        ]
        
        # Rotate and translate to world position
        corners = []
        for lc in local_corners:
            # Rotate
            rx = lc.x * cos_r - lc.y * sin_r
            ry = lc.x * sin_r + lc.y * cos_r
            # Translate
            corners.append(Vec2(rx + self.center.x, ry + self.center.y))
        
        return corners


def line_intersection(p1, p2, p3, p4):
    """
    Find intersection point of two line segments.
    p1-p2: first line segment
    p3-p4: second line segment
    Returns (intersection_point, t1, t2) or (None, None, None) if no intersection.
    t1, t2 are the parameters along each line (0 to 1 means within segment).
    """
    x1, y1 = p1.x, p1.y
    x2, y2 = p2.x, p2.y
    x3, y3 = p3.x, p3.y
    x4, y4 = p4.x, p4.y
    
    denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    
    if abs(denom) < 1e-10:
        return None, None, None
    
    t1 = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denom
    t2 = ((x1 - x3) * (y1 - y2) - (y1 - y3) * (x1 - x2)) / denom
    
    if 0 <= t1 <= 1 and 0 <= t2 <= 1:
        intersection = Vec2(x1 + t1 * (x2 - x1), y1 + t1 * (y2 - y1))
        return intersection, t1, t2
    
    return None, None, None


def reflect_direction(direction, normal):
    """Reflect a direction vector across a normal vector."""
    # Reflection formula: d - 2(d·n)n
    d_dot_n = direction.dot(normal)
    reflected = direction - normal * (2 * d_dot_n)
    return reflected.normalize()


def find_edge_normal(edge_start, edge_end):
    """Compute the outward normal of an edge."""
    edge_vec = edge_end - edge_start
    # Perpendicular vector (rotate 90 degrees)
    normal = Vec2(-edge_vec.y, edge_vec.x)
    return normal.normalize()


def trace_ray(start, direction, boxes, bounds_width, bounds_height, max_bounces=1000):
    """
    Trace a ray through the scene, collecting colored line segments.
    Returns a list of (start_point, end_point, color) tuples.
    """
    segments = []
    current_pos = start
    current_dir = direction
    current_color = "white"
    
    for _ in range(max_bounces):
        # Find the closest intersection with any box or boundary
        closest_t = float('inf')
        closest_point = None
        closest_box = None
        closest_edge_idx = None
        hit_boundary = False
        
        # Check intersection with boxes
        for box in boxes:
            for edge_idx, (edge_start, edge_end) in enumerate(box.edges):
                # Create a ray segment far enough to hit anything
                ray_end = current_pos + current_dir * 10000
                intersection, t_ray, t_edge = line_intersection(current_pos, ray_end, edge_start, edge_end)
                
                if intersection and t_ray > 1e-6 and t_ray < closest_t:
                    closest_t = t_ray
                    closest_point = intersection
                    closest_box = box
                    closest_edge_idx = edge_idx
                    hit_boundary = False
        
        # Check intersection with bounding box
        ray_end = current_pos + current_dir * 10000
        boundary_edges = [
            (Vec2(0, 0), Vec2(bounds_width, 0)),  # Bottom
            (Vec2(bounds_width, 0), Vec2(bounds_width, bounds_height)),  # Right
            (Vec2(bounds_width, bounds_height), Vec2(0, bounds_height)),  # Top
            (Vec2(0, bounds_height), Vec2(0, 0))  # Left
        ]
        
        for edge_start, edge_end in boundary_edges:
            intersection, t_ray, t_edge = line_intersection(current_pos, ray_end, edge_start, edge_end)
            
            if intersection and t_ray > 1e-6 and t_ray < closest_t:
                closest_t = t_ray
                closest_point = intersection
                closest_box = None
                hit_boundary = True
        
        if closest_point is None:
            # No intersection found (shouldn't happen)
            break
        
        # Add the segment
        segments.append((current_pos, closest_point, current_color))
        
        if hit_boundary:
            # Ray exits the bounding box
            break
        
        # Bounce off the box
        if closest_box:
            # Get the edge normal
            edge_start, edge_end = closest_box.edges[closest_edge_idx]
            normal = find_edge_normal(edge_start, edge_end)
            
            # Reflect the direction
            current_dir = reflect_direction(current_dir, normal)
            current_pos = closest_point
            current_color = closest_box.color
    
    return segments


def parse_input(filename):
    """Parse the input file and return bounds, boxes, and rays."""
    with open(filename, 'r') as f:
        lines = f.readlines()
    
    bounds_width = None
    bounds_height = None
    box_width = None
    box_height = None
    boxes = []
    rays = []
    
    for line in lines:
        line = line.strip()
        if not line or line.startswith('#'):
            continue
        
        parts = line.split()
        
        if parts[0] == 'BOUNDS':
            bounds_width = float(parts[1])
            bounds_height = float(parts[2])
        elif parts[0] == 'BOX':
            box_width = float(parts[1])
            box_height = float(parts[2])
        elif parts[0] == 'COLORED_BOX':
            center_x = float(parts[1])
            center_y = float(parts[2])
            rotation = float(parts[3])
            color = parts[4]
            boxes.append(Box(center_x, center_y, box_width, box_height, rotation, color))
        elif parts[0] == 'RAY':
            start_x = float(parts[1])
            start_y = float(parts[2])
            rays.append(Vec2(start_x, start_y))
    
    return bounds_width, bounds_height, boxes, rays


def write_output(filename, segments):
    """Write the segments to an output file."""
    with open(filename, 'w') as f:
        for start, end, color in segments:
            f.write(f"SEGMENT {start.x} {start.y} {end.x} {end.y} {color}\n")


def main():
    if len(sys.argv) < 3:
        print("Usage: python ray_bounce.py <input_file> <output_file>")
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_file = sys.argv[2]
    
    # Parse input
    bounds_width, bounds_height, boxes, rays = parse_input(input_file)
    
    # Initial direction: negative y-axis
    initial_direction = Vec2(0, -1)
    
    # Trace all rays
    all_segments = []
    for ray_start in rays:
        segments = trace_ray(ray_start, initial_direction, boxes, bounds_width, bounds_height)
        all_segments.extend(segments)
    
    # Write output
    write_output(output_file, all_segments)
    print(f"Processed {len(rays)} rays, generated {len(all_segments)} segments.")
    print(f"Output written to {output_file}")


if __name__ == "__main__":
    main()