//
//  Timing.h
//  NIOOApps
//
//  Created by Jayson Haebich on 08/05/2019.
//

#ifndef Timing_h
#define Timing_h


class Timing
{
public:
    Timing();
    Timing(float _start, float _end);
    
    int getStartSecond();
    int getStartMinute();
    int getEndSecond();
    int getEndMinute();
    int geTotalSeconds()
    {
        return endTime - startTime;
    }
    bool operator < (const Timing & t)
    {
        (this->startTime < t.startTime) ? true : false;
    }
    
    bool operator > (const Timing & t)
    {
        (this->startTime > t.startTime) ? true : false;
    }
    float getLength();
    bool hasStarted( float checkTime );
    
    float getTimeTillEnd(    float time );
    float getTimeSinceStart( float time );
    
    
    float startTime, endTime;
    int startSecond, startMinute;
    int endSecond, endMinute;
    
    static float toSeconds( int min, int sec ) { return 60 * min + sec; }
private:
    
    
};

#endif /* Timing_h */
