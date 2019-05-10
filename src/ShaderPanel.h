//
//  ShaderPanel.h
//  NIOOApps
//
//  Created by Jayson Haebich on 08/05/2019.
//

#ifndef ShaderPanel_h
#define ShaderPanel_h
#include "Timing.h"
#include "ofMain.h"

class ShaderPanel
{
public:
    
    void loadShader( string shaderPath, float startTime, float endTime );
    void draw();
private:
    
    Timing _timeline;
};

#endif /* ShaderPanel_h */
