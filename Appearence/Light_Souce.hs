lightSourceFound xs = lighting $= Enabled
                      position (Light 0) $= Vertex4 1 0.4 0.8 1
                      ambient (Light 0) $= Color4 1 1 1 1
                      -- specular (Light 0) $= Color4 1 1 1 1
                      -- diffuse (Light 0) $= Color4 1 1 1 1
                      light (Light 0) $= Enabled
                      -- currentColor $= Color4 0 0 0 0
                      clearColor $= Color4 0 0 0 1

