
def neighbors(point):
    temp = []
    fin = []
    for i in point:
        temp.append([i-1,i+1])
    for i in range(len(temp)):
        for j in temp[i]:
            new_point = list(point)
            new_point[i] = j
            fin.append(tuple(new_point))
   
    return fin

def clean_neighbors(point_list):
    set_current = set(point_list)
    possible_points = []
    for k in point_list:
        possible_points += neighbors(k)
    set_points = set(possible_points)
    return list(set_points.difference(set_current))

def clean_shapes(shapes):
    final_shapes = []
    actual = []
    for i in range(len(shapes)):
        check_shape = shapes[i]
        set_shape = set(check_shape)
        if not (set_shape in final_shapes):
            final_shapes.append(set_shape)
            actual.append(list(set_shape))
    return actual

def iterate(prev_iter):
    new_shapes = []
    for k in prev_iter:
        new_points = clean_neighbors(k)
        for i in new_points:
            new_shapes.append(k + [i])
    return clean_shapes(new_shapes)

def main_loop(n = 5, size=2):
    start = [0]*size
    shapes = [ [ [ tuple(start) ] ] ]
    for i in range(n):
        h = shapes[i]
        shapes.append(iterate(h))
        print(len(h))
        