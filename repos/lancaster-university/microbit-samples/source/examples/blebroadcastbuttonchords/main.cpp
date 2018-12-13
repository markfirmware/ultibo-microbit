#include "MicroBit.h"
#define SHORT_VERSION "v20181213"
MicroBit uBit;
MicroBitSerial serial(USBTX, USBRX);
uint8_t tx_power_level = 6;

#define FLAGS_NONE        0
#define FLAGS_BOTH        1
#define FLAGS_PI_ASSIGNED 2
struct
{
    uint8_t manufacturer[2];
    uint8_t signature[3];
    uint8_t piBluetoothMacLower[3];
    uint8_t flags;
    uint8_t counter;
    uint8_t chords[16];
} BleAdvPacketC8 = {0xff, 0xff,
                    0x78, 0xf3, 0xc8,
                    0x00, 0x00, 0x00,
                    0,
                    0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

uint8_t buttonsState = 0;
uint8_t eventCounter = 0;

ManagedString shortVersion = SHORT_VERSION;

char digit(uint8_t n)
{
    return '0' + n;
}

void fail(ManagedString message)
{
    uBit.display.scroll("FAIL " + message, 200);
}

void startAdvertising()
{
    int connectable = 0;
    int interval = 160;
    uBit.bleManager.setTransmitPower(tx_power_level);
    uBit.bleManager.ble->setAdvertisingType(connectable ? GapAdvertisingParams::ADV_CONNECTABLE_UNDIRECTED : GapAdvertisingParams::ADV_NON_CONNECTABLE_UNDIRECTED);
    uBit.bleManager.ble->setAdvertisingInterval(interval);
    uBit.bleManager.ble->clearAdvertisingPayload();
    uBit.bleManager.ble->accumulateAdvertisingPayload(GapAdvertisingData::BREDR_NOT_SUPPORTED | GapAdvertisingData::LE_GENERAL_DISCOVERABLE);
    uBit.bleManager.ble->accumulateAdvertisingPayload(GapAdvertisingData::MANUFACTURER_SPECIFIC_DATA, (uint8_t*) &BleAdvPacketC8, sizeof(BleAdvPacketC8));
    uBit.bleManager.ble->startAdvertising();
    if (sizeof(BleAdvPacketC8) == 26)
        uBit.display.scrollAsync("C8 ULTIBO C8", 90);
    else
        fail("ULTIBO PACK");
}

char text[] = "?";

void updateBroadcastVersion()
{
    if ((BleAdvPacketC8.counter & 0x80) == 0)
    {
        for (uint32_t last = -1, i = 0, freeOffset = (BleAdvPacketC8.counter + 3) / 4; last != 0 && freeOffset < sizeof(BleAdvPacketC8.chords);)
        {
            last = shortVersion.charAt(i++);
            BleAdvPacketC8.chords[freeOffset++] = last;
        }
    }
}

void onButton(MicroBitEvent e)
{
    uint8_t mask = 0;
    uint8_t prev = buttonsState;
    if (e.source == MICROBIT_ID_BUTTON_A)
        mask = 0x01;
    else if (e.source == MICROBIT_ID_BUTTON_B)
        mask = 0x02;
    if (e.value == MICROBIT_BUTTON_EVT_DOWN)
        buttonsState |= mask;
    else if (e.value == MICROBIT_BUTTON_EVT_UP)
        buttonsState &= ~mask;

    if (buttonsState != prev)
    {
        uBit.bleManager.ble->clearAdvertisingPayload();
        uBit.bleManager.ble->accumulateAdvertisingPayload(GapAdvertisingData::BREDR_NOT_SUPPORTED | GapAdvertisingData::LE_GENERAL_DISCOVERABLE);
        if (buttonsState != 3)
        {
            if (eventCounter == 255)
            {
                eventCounter = 128;
            }
            else
            {
               eventCounter = eventCounter + 1;
            }
            BleAdvPacketC8.flags = FLAGS_NONE;
            BleAdvPacketC8.counter = eventCounter;
            for (int i = sizeof 15; i >= 1; i--)
            {
                BleAdvPacketC8.chords[i] = ((BleAdvPacketC8.chords[i] >> 2) & 0x3f) | ((BleAdvPacketC8.chords[i - 1] & 0x03) << 6);
            }
            BleAdvPacketC8.chords[0] = ((BleAdvPacketC8.chords[0] >> 2) & 0x3f) | (buttonsState << 6);
            updateBroadcastVersion();
        }
        else
        {
            BleAdvPacketC8.flags = FLAGS_BOTH;
        }
        uBit.bleManager.ble->accumulateAdvertisingPayload(GapAdvertisingData::MANUFACTURER_SPECIFIC_DATA, (uint8_t*) &BleAdvPacketC8, sizeof(BleAdvPacketC8));
        switch (buttonsState)
        {
            case 0:  text[0] = ' ';
                     break;
            case 1:  text[0] = 'A';
                     break;
            case 2:  text[0] = 'B';
                     break;
            case 3:  text[0] = '2';
                     break;
            default: text[0] = '?';
        }
        uBit.display.printAsync(text);
    }
}

int main()
{
    uBit.init();
    uBit.sleep(1*1000);
    serial.printf("{\"repository\":\"ultibo-microbit\",\"project\":\"repos/lancaster-university/microbit-samples/source/examples/blebroadcastbuttonchords\",\"semanticApiVersion:\"0.1\"}\n");
    uBit.messageBus.listen(MICROBIT_ID_BUTTON_A, MICROBIT_EVT_ANY, onButton);
    uBit.messageBus.listen(MICROBIT_ID_BUTTON_B, MICROBIT_EVT_ANY, onButton);
    updateBroadcastVersion();
    startAdvertising();
    release_fiber();
}
