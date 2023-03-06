module Arkham.Location.Cards.TrophyRoomSpectral
  ( trophyRoomSpectral
  , TrophyRoomSpectral(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Message
import Arkham.Projection

newtype TrophyRoomSpectral = TrophyRoomSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trophyRoomSpectral :: LocationCard TrophyRoomSpectral
trophyRoomSpectral =
  location TrophyRoomSpectral Cards.trophyRoomSpectral 2 (PerPlayer 1)

instance HasAbilities TrophyRoomSpectral where
  getAbilities (TrophyRoomSpectral attrs) = withBaseAbilities
    attrs
    [ haunted
        "Lose 2 resources. For each resource you cannot lose from this effect, take 1 horror."
        attrs
        1
    ]

instance RunMessage TrophyRoomSpectral where
  runMessage msg l@(TrophyRoomSpectral attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      resources <- field InvestigatorResources iid
      let
        resourcesToLose = min resources 2
        horrorToTake = 2 - resourcesToLose
      pushAll
        $ LoseResources iid (toSource attrs) resourcesToLose
        : [ InvestigatorAssignDamage
              iid
              (toSource attrs)
              DamageAny
              0
              horrorToTake
          | horrorToTake > 0
          ]
      pure l
    _ -> TrophyRoomSpectral <$> runMessage msg attrs
