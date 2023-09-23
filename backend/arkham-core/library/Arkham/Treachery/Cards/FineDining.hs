module Arkham.Treachery.Cards.FineDining (
  fineDining,
  FineDining (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype FineDining = FineDining TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fineDining :: TreacheryCard FineDining
fineDining = treachery FineDining Cards.fineDining

instance RunMessage FineDining where
  runMessage msg t@(FineDining attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      clueCount <- field InvestigatorClues iid
      bystanders <- selectListMap AssetTarget $ AssetWithTrait Bystander
      let damageMsg = InvestigatorAssignDamage iid source DamageAny 1 1
      push
        $ if clueCount > 0 && notNull bystanders
          then
            chooseOne
              iid
              [ Label
                  "Place 1 of your clues on a Bystander asset in play"
                  [ chooseOne
                      iid
                      [ TargetLabel
                        target
                        [InvestigatorSpendClues iid 1, PlaceClues (toSource attrs) target 1]
                      | target <- bystanders
                      ]
                  ]
              , Label "Take 1 horror and 1 damage" [damageMsg]
              ]
          else damageMsg
      pure t
    _ -> FineDining <$> runMessage msg attrs
