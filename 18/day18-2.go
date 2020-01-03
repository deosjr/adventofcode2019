package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"math"
	"os"
	"sort"
	"strings"
	"unicode"
)

type vault struct {
	coords map[point]tile
}

type point struct {
	x int
	y int
}

type tile struct {
	point point
	char  rune
}

type node struct {
	point point
	// []rune is unhashable...
	keys string
}

func haveKeyForDoor(keys string, door rune) bool {
	if !unicode.IsUpper(door) {
		return true
	}
	return strings.ContainsRune(strings.ToUpper(keys), door)
}

func get_neighbours(p point) []point {
	return []point{
		{p.x + 1, p.y},
		{p.x - 1, p.y},
		{p.x, p.y + 1},
		{p.x, p.y - 1}}
}

func (v vault) Neighbours(nx Node) []Node {
	n := nx.(node)
	neighbours := []Node{}
	npoints := get_neighbours(n.point)
	for _, p := range npoints {
		if t, ok := v.coords[p]; ok {
			if haveKeyForDoor(n.keys, t.char) {
				neighbours = append(neighbours, node{t.point, n.keys})
			}
		}
	}
	return neighbours
}

func (v vault) G(n, neighbour Node) float64 {
	return 1
}

func (v vault) H(nx, gx Node) float64 {
	p := nx.(node).point
	q := gx.(node).point
	dx := float64(q.x - p.x)
	dy := float64(q.y - p.y)
	return math.Abs(dx) + math.Abs(dy)
}

func (v vault) floodfill_test(start node) map[rune]string {
	type keyReq struct {
		point point
		keys  string
	}
	keys := map[rune]string{}
	considered := map[point]bool{start.point: true}
	toVisit := []keyReq{{point: start.point, keys: ""}}
	for len(toVisit) > 0 {
		p := toVisit[0]
		toVisit = toVisit[1:]
		for _, n := range get_neighbours(p.point) {
			if _, ok := considered[n]; ok {
				continue
			}
			considered[n] = true
			t, ok := v.coords[n]
			if !ok {
				continue
			}
			if unicode.IsLower(t.char) {
				keys[t.char] = p.keys
			}
			k := p.keys
			if t.char != '.' {
				k = fmt.Sprintf("%s%c", k, t.char)
			}
			toVisit = append(toVisit, keyReq{point: n, keys: k})
		}
	}
	return keys
}

func (v vault) floodfill(start node) []rune {
	keys := []rune{}
	considered := map[point]bool{start.point: true}
	toVisit := []point{start.point}
	for len(toVisit) > 0 {
		p := toVisit[0]
		toVisit = toVisit[1:]
		for _, n := range get_neighbours(p) {
			if _, ok := considered[n]; ok {
				continue
			}
			considered[n] = true
			t, ok := v.coords[n]
			if !ok {
				continue
			}
			if !haveKeyForDoor(start.keys, t.char) {
				continue
			}
			if unicode.IsLower(t.char) && !strings.ContainsRune(start.keys, t.char) {
				keys = append(keys, t.char)
			} else {
				toVisit = append(toVisit, n)
			}
		}
	}
	return keys
}

func main() {
	file, err := os.Open("day18.input")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	m := map[point]tile{}

	var start node
	keys := map[rune]point{}

	scanner := bufio.NewScanner(file)
	y := 0
	for scanner.Scan() {
		x := 0
		for _, r := range scanner.Text() {
			p := point{x, y}
			x++
			if r == '#' {
				continue
			}
			t := tile{point: p}
			if r == '@' {
				start = node{point: p}
			} else if r != '.' {
				t.char = r
			}
			if unicode.IsLower(r) {
				keys[r] = p
			}
			m[p] = t
		}
		y++
	}

	if err := scanner.Err(); err != nil {
		panic(err)
	}

	// modify the maze as per part 2 instructions
	delete(m, start.point)
	for _, n := range get_neighbours(start.point) {
		delete(m, n)
	}

	vault := vault{coords: m}

	type pair struct {
		from rune
		to   rune
	}

	type route struct {
		distance  int
		obstacles string
	}

	distances := map[pair]int{}
	routes := map[pair]route{}

	for from := 'a'; from < 'z'; from++ {
		for to := 'z'; to > from; to-- {
			start := node{keys[from], "abcdefghijklmnopqrstuvwxyz"}
			end := node{keys[to], "abcdefghijklmnopqrstuvwxyz"}
			route, _ := FindRoute(vault, start, end)
			distances[pair{from, to}] = len(route) - 1
		}
	}

	// NOTE: 0-9 < a-z in ascii
	startingPositions := []point{
		{start.point.x - 1, start.point.y - 1},
		{start.point.x + 1, start.point.y - 1},
		{start.point.x - 1, start.point.y + 1},
		{start.point.x + 1, start.point.y + 1},
	}
	for r, point := range startingPositions {
		robot := rune(r + 48)
		for key := 'a'; key <= 'z'; key++ {
			start := node{point, "abcdefghijklmnopqrstuvwxyz"}
			end := node{keys[key], "abcdefghijklmnopqrstuvwxyz"}
			route, _ := FindRoute(vault, start, end)
			distances[pair{robot, key}] = len(route) - 1
		}
		floodfill := vault.floodfill_test(node{point: point, keys: ""})
		for k, v := range floodfill {
			p := pair{from: robot, to: k}
			routes[p] = route{distance: distances[p], obstacles: v}
		}
	}

	for c := 'a'; c <= 'z'; c++ {
		floodfill := vault.floodfill_test(node{point: keys[c], keys: ""})
		for k, v := range floodfill {
			if k < c {
				continue
			}
			p := pair{from: c, to: k}
			routes[p] = route{distance: distances[p], obstacles: v}
		}
	}

	pq := priorityQueue2{}
	heap.Init(&pq)

	start.keys = ""
	item := &pqItem2{
		node:      node{point: start.point, keys: ""},
		pathSoFar: 0,
		sortScore: 0,
		lastKeys:  []rune{'0', '1', '2', '3'},
	}
	heap.Push(&pq, item)

	ans := 99999

	visited := map[node]int{}

	for pq.Len() != 0 {
		item := heap.Pop(&pq).(*pqItem2)
		start := item.node
		for k, _ := range keys {
			if strings.ContainsRune(start.keys, k) {
				continue
			}
			for _, lastKey := range item.lastKeys {
				p := pair{from: lastKey, to: k}
				if k < lastKey {
					p = pair{from: k, to: lastKey}
				}
				route, ok := routes[p]
				if !ok {
					continue
				}
				for _, r := range route.obstacles {
					if !haveKeyForDoor(start.keys, r) {
						continue
					}
					if unicode.IsLower(r) && !strings.ContainsRune(start.keys, r) {
						continue
					}
				}
				sum := item.pathSoFar + route.distance
				if sum > ans {
					continue
				}
				newKeys := fmt.Sprintf("%s%c", start.keys, k)
				if len(newKeys) == len(keys) {
					if sum < ans {
						ans = sum
						fmt.Println(ans)
					}
					continue
				}

				sortedNode := node{point: keys[k], keys: sortStringByCharacter(newKeys)}
				if d, ok := visited[sortedNode]; ok && d < sum {
					continue
				}
				visited[sortedNode] = sum

				newLastKeys := []rune{}
				for _, lk := range item.lastKeys {
					if lk == lastKey {
						continue
					}
					newLastKeys = append(newLastKeys, lk)
				}
				newLastKeys = append(newLastKeys, k)

				newItem := &pqItem2{
					node:      node{point: keys[k], keys: newKeys},
					pathSoFar: sum,
					sortScore: float64(sum) + float64(150*(len(keys)-len(newKeys))),
					lastKeys:  newLastKeys,
				}
				heap.Push(&pq, newItem)
			}
		}
	}

	fmt.Printf("Part 2: %d\n", ans)
}

func stringToRuneSlice(s string) []rune {
	var r []rune
	for _, runeValue := range s {
		r = append(r, runeValue)
	}
	return r
}

func sortStringByCharacter(s string) string {
	r := stringToRuneSlice(s)
	sort.Slice(r, func(i, j int) bool {
		return r[i] < r[j]
	})
	return string(r)
}

type pqItem2 struct {
	node      node
	pathSoFar int
	sortScore float64
	index     int
	lastKeys  []rune
}

type priorityQueue2 []*pqItem2

func (pq priorityQueue2) Len() int { return len(pq) }

func (pq priorityQueue2) Less(i, j int) bool {
	return pq[i].sortScore < pq[j].sortScore
}

func (pq priorityQueue2) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
	pq[i].index = i
	pq[j].index = j
}

func (pq *priorityQueue2) Push(x interface{}) {
	n := len(*pq)
	item := x.(*pqItem2)
	item.index = n
	*pq = append(*pq, item)
}

func (pq *priorityQueue2) Pop() interface{} {
	old := *pq
	n := len(old)
	item := old[n-1]
	item.index = -1
	*pq = old[0 : n-1]
	return item
}
