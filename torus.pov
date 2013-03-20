torus {
4
1
pigment { color rgb <1,1,0> }
rotate <-90,0,0>
}
camera {
up < 0, 2, 1 >
direction < 1,1,1>
right < 1, 0, 1 >
location < 0, 0.1, -25>
look_at < 0, 0, 0>
angle 30
}
light_source {
  <300,300,-1000>
  color rgb <1,1,0>
}
