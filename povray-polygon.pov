camera {
    orthographic
    location <0, -1, -2>
    right 1.3 * 4/3 * x
    up 1.3 * y
    look_at <0, 1, 0>
    direction<-3,2,0> 
    angle 45
  }
  light_source { <25, 25, -100> color rgb 1 }
polygon {
    8,
    <0.45, 0.0,0>,
    <0.30, 1.0,0>, 
    <0.40, 1.0,0>,
    <0.55, 0.1,0>,
    <0.70, 1.0,0>,
    <0.80, 1.0,0>,
    <0.65, 0.0,0>,
    <0.45, 0.0,0>
    pigment { color rgb <1, 0, 0> }
  }
