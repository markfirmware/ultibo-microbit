#include "MicroBit.h"

MicroBit uBit;
MicroBitSerial serial(USBTX, USBRX);

int main()
{
    uBit.init();
    uBit.sleep(1*1000);
    serial.printf("{\"repository\":\"microbit-samples\",\"project\":\"source/examples/peripheral\",\"semanticApiVersion:\"0.1\"}\n");
    while (1)
    {
        uBit.sleep(1*1000);
        serial.printf("{\"clock\":%d}\n", uBit.systemTime() / 1000);
        uBit.display.scrollAsync(".", 100);
    }
    release_fiber();
}

