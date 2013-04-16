cone {
    <0, 0, 0>, 
    0.0    
    <0, 3, -4>,
    1.0  
pigment { color rgb <0,1,0> }
}
camera {
up< 0, 2, 1 >
direction < 0,1,0>
location <0,0,-10>
look_at <0,3,3>
angle 50
right< 1, 0, 1 >
}
light_source {
<5, 10, -15> 
color rgb <1,1,1>
}
