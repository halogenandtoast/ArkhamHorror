module Arkham.Treachery.Cards.FineDining
  ( fineDining
  , FineDining(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Query
import Arkham.Target
import Arkham.Trait
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype FineDining = FineDining TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fineDining :: TreacheryCard FineDining
fineDining = treachery FineDining Cards.fineDining

instance TreacheryRunner env => RunMessage env FineDining where
  runMessage msg t@(FineDining attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      clueCount <- unClueCount <$> getCount iid
      bystanders <- selectListMap AssetTarget $ AssetWithTrait Bystander
      let damageMsg = InvestigatorAssignDamage iid source DamageAny 1 1
      t <$ push
        (if clueCount > 0 && notNull bystanders
          then chooseOne
            iid
            [ Label
              "Place 1 of your clues on a Bystander asset in play"
              [ chooseOne
                  iid
                  [ TargetLabel
                      target
                      [InvestigatorSpendClues iid 1, PlaceClues target 1]
                  | target <- bystanders
                  ]
              ]
            , Label "Take 1 horror and 1 damage" [damageMsg]
            ]
          else damageMsg
        )
    _ -> FineDining <$> runMessage msg attrs
