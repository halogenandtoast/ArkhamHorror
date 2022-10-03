module Arkham.Location.Cards.EztliExhibit
  ( eztliExhibit
  , EztliExhibit(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Token

newtype EztliExhibit = EztliExhibit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eztliExhibit :: LocationCard EztliExhibit
eztliExhibit = location EztliExhibit Cards.eztliExhibit 3 (PerPlayer 2)

instance HasAbilities EztliExhibit where
  getAbilities (EztliExhibit attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
        attrs
        1
        (DuringSkillTest $ WhileInvestigating $ LocationWithId $ toId attrs)
      $ ForcedAbility
      $ RevealChaosToken Timing.After You
      $ TokenMatchesAny
      $ map TokenFaceIs [Skull, Cultist, Tablet, ElderThing, AutoFail]
    ]

instance RunMessage EztliExhibit where
  runMessage msg l@(EztliExhibit attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ InvestigatorAssignDamage iid source DamageAny 0 1
      pure l
    _ -> EztliExhibit <$> runMessage msg attrs
