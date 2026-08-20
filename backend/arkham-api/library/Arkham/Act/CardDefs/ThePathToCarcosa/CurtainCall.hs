module Arkham.Act.CardDefs.ThePathToCarcosa.CurtainCall where

import Arkham.Act.CardDefs.Import

awakening :: CardDef
awakening = act "03046" "Awakening" 1 CurtainCall

theStrangerACityAflame :: CardDef
theStrangerACityAflame = (act "03047a" "The Stranger" 2 CurtainCall) {cdOtherSide = Just "03047ab"}

theStrangerThePathIsMine :: CardDef
theStrangerThePathIsMine = act "03047b" "The Stranger" 2 CurtainCall

theStrangerTheShoresOfHali :: CardDef
theStrangerTheShoresOfHali = act "03047c" "The Stranger" 2 CurtainCall

curtainCall :: CardDef
curtainCall = act "03048" "Curtain Call" 3 CurtainCall
