polygon {
    5,
    <0.0,0.0,0.0>
         ,<1.0,-1.0, 0.0>
         ,<-1.0,1.0, 0.0>
         ,<1.0,-2.0, 0.0>
         ,<1.0,2.0, 0.0>
    texture {
      finish { ambient 1 diffuse 0 
 }
      pigment { color rgb <1, 1, 0>
 }
  } 
  }



camera {
location <0,0,-10>
look_at <0,0.5,0>
direction < 1,0,1>
angle 120
up< 1, 0, 0 >
right< 0, 1, 0 >
}

light_source {
  <0,10,-10>
  color rgb <1,1,0>
}
