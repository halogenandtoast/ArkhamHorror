module Arkham.Location.Cards.SecretPassage
  ( secretPassage
  , SecretPassage(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Criteria
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype SecretPassage = SecretPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretPassage :: LocationCard SecretPassage
secretPassage = locationWith
  SecretPassage
  Cards.secretPassage
  5
  (PerPlayer 1)
  (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasAbilities SecretPassage where
  getAbilities (SecretPassage attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
        attrs
        1
        (Negate $ InvestigatorExists
          (investigatorAt (toId attrs) <> InvestigatorWithSupply Rope)
        )
      $ ForcedAbility
      $ Enters Timing.After You
      $ LocationWithId
      $ toId attrs
    ]

instance RunMessage SecretPassage where
  runMessage msg l@(SecretPassage attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      push $ chooseOne
        iid
        [ Label
          "Take 1 horror and 1 damage"
          [InvestigatorAssignDamage iid source DamageAny 1 1]
        , Label "Place 1 doom on secret passage" [PlaceDoom (toTarget attrs) 1]
        ]

      pure l
    _ -> SecretPassage <$> runMessage msg attrs
