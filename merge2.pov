sphere {
<0, 0, -2>
3
pigment { color rgb <0,1,1> }  
}
cylinder {
<0, 0, -2>
<0, 10, -2>
1.5
pigment { color rgb <1,0,0> }
}
camera {
up< 1, -2, 3 >
direction < 1,1,1>
location <2,-9,-10>
look_at <-1,0,0>
angle 60
right< 1, 1, 0.2 >
}
light_source {
<0, 10, -10>
color rgb <1,1,1>
}
