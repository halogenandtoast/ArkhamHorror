import base from '@/locales/zh-cn/theDreamEaters/base.json'
import flavorText from '@/locales/zh-cn/theDreamEaters/flavorText.json'
import beyondTheGatesOfSleep from '@/locales/zh-cn/theDreamEaters/scenarios/beyondTheGatesOfSleep.json'
import beyondTheGatesOfSleepFlavorText from '@/locales/zh-cn/theDreamEaters/scenarios/beyondTheGatesOfSleepFlavorText.json'
import wakingNightmare from '@/locales/zh-cn/theDreamEaters/scenarios/wakingNightmare.json'
import wakingNightmareFlavorText from '@/locales/zh-cn/theDreamEaters/scenarios/wakingNightmareFlavorText.json'
import theSearchForKadath from '@/locales/zh-cn/theDreamEaters/scenarios/theSearchForKadath.json'
import theSearchForKadathFlavorText from '@/locales/zh-cn/theDreamEaters/scenarios/theSearchForKadathFlavorText.json'
import aThousandShapesOfHorror from '@/locales/zh-cn/theDreamEaters/scenarios/aThousandShapesOfHorror.json'
import darkSideOfTheMoon from '@/locales/zh-cn/theDreamEaters/scenarios/darkSideOfTheMoon.json'
import pointOfNoReturn from '@/locales/zh-cn/theDreamEaters/scenarios/pointOfNoReturn.json'
import whereTheGodsDwell from '@/locales/zh-cn/theDreamEaters/scenarios/whereTheGodsDwell.json'
import weaverOfTheCosmos from '@/locales/zh-cn/theDreamEaters/scenarios/weaverOfTheCosmos.json'

export default {
  ...base,
  ...flavorText,
  beyondTheGatesOfSleep: {...beyondTheGatesOfSleep, ...beyondTheGatesOfSleepFlavorText},
  wakingNightmare: {...wakingNightmare, ...wakingNightmareFlavorText},
  theSearchForKadath: {...theSearchForKadath, ...theSearchForKadathFlavorText},
  aThousandShapesOfHorror,
  darkSideOfTheMoon,
  pointOfNoReturn,
  whereTheGodsDwell,
  weaverOfTheCosmos,
}
