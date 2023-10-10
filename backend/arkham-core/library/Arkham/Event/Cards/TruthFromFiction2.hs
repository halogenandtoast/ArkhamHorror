module Arkham.Event.Cards.TruthFromFiction2 (
  truthFromFiction2,
  TruthFromFiction2 (..),
) where

import Arkham.Prelude

import Arkham.Asset.Uses qualified as Uses
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher

newtype TruthFromFiction2 = TruthFromFiction2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

truthFromFiction2 :: EventCard TruthFromFiction2
truthFromFiction2 = event TruthFromFiction2 Cards.truthFromFiction2

instance RunMessage TruthFromFiction2 where
  runMessage msg e@(TruthFromFiction2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      clueOnLocation <- selectAny $ locationWithInvestigator iid <> LocationWithAnyClues
      assets <- selectList $ AssetControlledBy (colocatedWith iid) <> AssetWithUseType Uses.Secret
      let n = if clueOnLocation then 3 else 2
      player <- getPlayer iid
      pushAll
        $ replicate n
        $ chooseOne player [targetLabel aid [AddUses aid Uses.Secret 1] | aid <- assets]
      pure e
    _ -> TruthFromFiction2 <$> runMessage msg attrs
