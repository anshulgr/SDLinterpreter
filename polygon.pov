polygon {
    4,
    <0.0,0.0,1.0>
    <5.0,0.0,1.0>
         ,<5.0,5.0, 1.0>
         ,<0.0,5.0, 1.0>
        
         
    texture {
      finish { ambient 1 diffuse 0 
 }
      pigment { color rgb <1, 1, 0>
 }
  } 
  }

//camera 
//{
//location <3,5,-10>
//up    <0,1,0>
//right  <1,0,0>
//look_at <0,2,1>
//direction < 0,0,-1>
//angle 90
 //}

camera {
location <2,5,-10>
look_at <0,0,0>
direction < 1,1,-1>
angle 90
up< 1, 2, 3 >
right< -1, 1, 0.2 >
}

light_source {
  <0,10,-10>
  color rgb <1,1,0>
}
