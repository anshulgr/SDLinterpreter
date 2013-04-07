cylinder {
<0, 1, 1>
<0, 10, -2>
0.5
pigment { color rgb <0,0,1> }
translate <-5, 2, 1>
rotate <0,2,0>
scale <2,1,0.5>
}
camera {
location <1, -2, -2>
look_at <0, 0, 0>
direction < 1,1,-1>
up < -1, 2, 3 >
right < 0, 0, 0 >
angle 90
}
light_source {
<10, 10, -10> color rgb <0,0,0>
}
