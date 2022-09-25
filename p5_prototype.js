const object = []; 

function setup() {
  object.length = 0;
  for (let i = 0; i < 100; i++) object.push({
    x: Math.random(),
    y: Math.random(),
    vx: (Math.random()-0.5)*0.01,
    vy: (Math.random()-0.5)*0.01,
  });
  createCanvas(400, 400);
}

function hash(a, b) {
  return fract( (1 + Math.sin(a * 314159 + b * 2653)) * 200);
}

function connection(a, b) {
  return hash(a, b) < 0.01;
}

function forceTowards(obj, cx, cy, a) {
  let dx = cx - obj.x, dy = cy - obj.y;
  let norm = Math.hypot(dx, dy);
  obj.vx += a * dx / norm;
  obj.vy += a * dy / norm;
  obj.vx *= 0.9995;
  obj.vy *= 0.9995;
}

function keepNearOrigin(obj) {
  let r = Math.hypot(obj.x - 0.5, obj.y - 0.5);
  forceTowards(obj, 0.5, 0.5, r**4 * 0.0005);
}

function keepApart(o1, o2, c) {
  let r = Math.hypot(o1.x - o2.x, o1.y - o2.y);
  let f = Math.min(r, 0.5) * c * 0.0001
        - Math.min(0.01, 1e-7 / r**2); // repulsive force
  forceTowards(o1, o2.x, o2.y, f);
  forceTowards(o2, o1.x, o1.y, f);
}

function step(quiet) {
  for (let obj of object) {
    keepNearOrigin(obj);
    obj.x += obj.vx; obj.y += obj.vy;
  }
  for (let i = 0; i < object.length - 1; i++) {
    let o1 = object[i]
    for (let j = i + 1; j < object.length; j++) {
      let o1 = object[i], o2 = object[j];
      let conn = connection(i, j);
      if (conn && quiet) {
        stroke("red");
        line(o1.x*400, o1.y*400, o2.x*400, o2.y*400)
      }
      keepApart(o1, o2, connection(i, j));
    }
  }
}

function draw() {
  background("black");
  fill("white");
  noStroke();
  for (let obj of object) {
    circle(obj.x * 400, obj.y * 400, 7);
  }
  for (let i = 0; i < 5; i++) step(!i);
}
