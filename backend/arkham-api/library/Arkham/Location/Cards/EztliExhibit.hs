module Arkham.Location.Cards.EztliExhibit (eztliExhibit) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher

newtype EztliExhibit = EztliExhibit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eztliExhibit :: LocationCard EztliExhibit
eztliExhibit = location EztliExhibit Cards.eztliExhibit 3 (PerPlayer 2)

instance HasAbilities EztliExhibit where
  getAbilities (EztliExhibit a) =
    extendRevealed1 a
      $ restricted a 1 (DuringSkillTest $ WhileInvestigating $ be a)
      $ forced
      $ RevealChaosToken #after You
      $ mapOneOf ChaosTokenFaceIs [Skull, Cultist, Tablet, ElderThing, AutoFail]

instance RunMessage EztliExhibit where
  runMessage msg l@(EztliExhibit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> EztliExhibit <$> liftRunMessage msg attrs
