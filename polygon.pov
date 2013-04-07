polygon {
    5,
    <0.0,0.0,0.0>
         ,<1.0,1.0, 0.0>
         ,<1.0,-1.0, 0.0>
         ,<3.0,1.0, 0.0>
         ,<3.0,-1.0, 0.0>
    texture {
      finish { ambient 1 diffuse 0 
 }
      pigment { color rgb <1, 1, 0>
 }
  } 
  }

camera {
location <1, -2, -2>
look_at <0, 5, 0>
direction < 1,1,-1>
up< 1, 2, 3 >
right< -1, 1, 0.2 >
angle 90
focal_point <0.20,1.5,-5.25>
aperture 0.7 
}
light_source {
  <0,10,-10>
  color rgb <1,1,1>
}
