module Arkham.Treachery.CardDefs.TheForgottenAge.TheDepthsOfYoth where

import Arkham.Treachery.CardDefs.Import

bathophobia :: CardDef
bathophobia =
  (treachery "04301" "Bathophobia" TheDepthsOfYoth 3)
    { cdCardTraits = singleton Terror
    }

childrenOfValusia :: CardDef
childrenOfValusia =
  (treachery "04299" "Children of Valusia" TheDepthsOfYoth 3)
    { cdCardTraits = singleton Scheme
    }

lightlessShadow :: CardDef
lightlessShadow =
  (treachery "04300" "Lightless Shadow" TheDepthsOfYoth 3)
    { cdCardTraits = singleton Terror
    }

serpentsIre :: CardDef
serpentsIre =
  (treachery "04302" "Serpent's Ire" TheDepthsOfYoth 2)
    { cdCardTraits = singleton Scheme
    }
