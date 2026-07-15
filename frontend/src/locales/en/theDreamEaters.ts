import base from '@/locales/en/theDreamEaters/base.json'
import flavorText from '@/locales/en/theDreamEaters/flavorText.json'
import beyondTheGatesOfSleep from '@/locales/en/theDreamEaters/scenarios/beyondTheGatesOfSleep.json'
import beyondTheGatesOfSleepFlavorText from '@/locales/en/theDreamEaters/scenarios/beyondTheGatesOfSleepFlavorText.json'
import wakingNightmare from '@/locales/en/theDreamEaters/scenarios/wakingNightmare.json'
import wakingNightmareFlavorText from '@/locales/en/theDreamEaters/scenarios/wakingNightmareFlavorText.json'
import theSearchForKadath from '@/locales/en/theDreamEaters/scenarios/theSearchForKadath.json'
import theSearchForKadathFlavorText from '@/locales/en/theDreamEaters/scenarios/theSearchForKadathFlavorText.json'
import aThousandShapesOfHorror from '@/locales/en/theDreamEaters/scenarios/aThousandShapesOfHorror.json'
import darkSideOfTheMoon from '@/locales/en/theDreamEaters/scenarios/darkSideOfTheMoon.json'
import pointOfNoReturn from '@/locales/en/theDreamEaters/scenarios/pointOfNoReturn.json'
import whereTheGodsDwell from '@/locales/en/theDreamEaters/scenarios/whereTheGodsDwell.json'
import weaverOfTheCosmos from '@/locales/en/theDreamEaters/scenarios/weaverOfTheCosmos.json'

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
