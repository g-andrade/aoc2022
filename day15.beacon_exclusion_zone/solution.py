#!/usr/bin/env python3
import pyqtree
import re
import sys

def calc_distance(pos_a, pos_b):
    x_a, y_a = pos_a
    x_b, y_b = pos_b
    return abs(x_a - x_b) + abs(y_a - y_b)

queried_y = None
if sys.argv[1] == 'part1':
    queried_y = int(sys.argv[2])
    print(f'Part 1, querying y={queried_y}')
else:
    assert sys.argv[1] == 'part2'
    max_distress_x = int(sys.argv[2])
    max_distress_y = int(sys.argv[3])
    max_distress_pos = max_distress_x, max_distress_y
    print(f'Part 2, querying for max distress pos {max_distress_pos}')

sensors_and_beacons = []
min_x, max_x = 0, 0
min_y, max_y = 0, 0

for line in sys.stdin.readlines():
    coords = re.match(
        r'^Sensor at x=(?P<sensor_x>-?[0-9]+), y=(?P<sensor_y>-?[0-9]+)'
        + r': closest beacon is at x=(?P<beacon_x>-?[0-9]+), y=(?P<beacon_y>-?[0-9]+)',
        line)

    sensor = int(coords.group('sensor_x')), int(coords.group('sensor_y'))
    beacon = int(coords.group('beacon_x')), int(coords.group('beacon_y'))
    distance = calc_distance(sensor, beacon)
    sensors_and_beacons.append((sensor, beacon, distance))

    sensor_x, sensor_y = sensor
    beacon_x, beacon_y = beacon
    min_x = min(min_x, sensor_x - distance, beacon_x)
    max_x = max(max_x, sensor_x + distance, beacon_x)
    min_y = min(min_y, sensor_y - distance, beacon_y)
    max_y = max(max_y, sensor_y + distance, beacon_y)

print(f'{len(sensors_and_beacons)} sensor-beacon pair(s) parsed')
print(f'min_x {min_x}, min_y {min_y}')
print(f'max_x {max_x}, max_y {max_y}')

qtree = pyqtree.Index(bbox=(min_x, min_y, max_x, max_y))
for sensor, beacon, distance in sensors_and_beacons:
    sensor_x, sensor_y = sensor
    sensor_bbox = (sensor_x - distance, sensor_y - distance, sensor_x + distance, sensor_y + distance)
    qtree.insert((sensor, beacon, distance), sensor_bbox)


if queried_y is not None:
    # Part 1
    matches = qtree.intersect((min_x, queried_y, max_x, queried_y))
    excluded_count = 0
    for x in range(min_x, max_x + 1):
        our_pos = x, queried_y
        for sensor, beacon, distance in matches:
            if calc_distance(our_pos, sensor) <= distance and our_pos != beacon:
                excluded_count += 1
                break

    print(f'excluded count for y={queried_y}: {excluded_count}')

else:
    # Part 2
    for y in range(0, max_distress_y + 1):
        x = 0
        if y % 100000 == 0:
            print(f'progress: {100 * y / (max_distress_y + 1)} %')

        while x < (max_distress_x + 1):
            our_pos = x, y
            # print(f'checking {our_pos}')

            matches = qtree.intersect((x, y, x, y))
            jumping = False
            for sensor, _beacon, distance in matches:
                if calc_distance(our_pos, sensor) <= distance:
                    sensor_x, sensor_y = sensor
                    y_distance = abs(sensor_y - y)
                    next_x = sensor_x + (distance - y_distance)
                    x = max(x, next_x)
                    jumping = True

            if not jumping and x < max_distress_x + 1:
                tuning_freq = (x * 4000000) + y
                print(f'found distress beacon at {(x, y)}; its tuning freq is {tuning_freq}')
                sys.exit(0)
            else:
                x += 1
