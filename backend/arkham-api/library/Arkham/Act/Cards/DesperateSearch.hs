module Arkham.Act.Cards.DesperateSearch (desperateSearch) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card
import Arkham.Constants
import Arkham.Deck qualified as Deck
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Scenarios.TheTwistedHollow.Helpers
import Arkham.Spawn
import Arkham.Trait (Trait (Lair))

newtype DesperateSearch = DesperateSearch ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desperateSearch :: ActCard DesperateSearch
desperateSearch = act (1, A) DesperateSearch Cards.desperateSearch Nothing

instance HasAbilities DesperateSearch where
  getAbilities = actAbilities1 \a ->
    restricted
      a
      ActAdvancement
      ( EachUndefeatedInvestigator
          (at_ $ LocationWithTrait Lair <> LocationWithInvestigator LeadInvestigator)
      )
      $ Objective
      $ triggered (RoundEnds #when)
      $ GroupClueCost (PerPlayer 3) (LocationWithTrait Lair)

instance HasModifiersFor DesperateSearch where
  getModifiersFor (DesperateSearch a) =
    modifySelect
      a
      (EnemyWithoutSpawn <> EnemyDrawnFrom Deck.EncounterDeck)
      [ForceSpawn (SpawnPlaced $ OutOfPlay PursuitZone)]

instance RunMessage DesperateSearch where
  runMessage msg a@(DesperateSearch attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      leadPlayer <- getLeadPlayer
      investigators <- getInvestigators
      sendRevelation leadPlayer (toJSON $ flipCard $ toCard attrs)
      scenarioI18n $ scope "codex.sigma" $ storyWithChooseOneM' (setTitle "title" >> p.green "body") do
        portraits investigators \iid -> codex iid attrs Sigma
      advanceActDeck attrs
      pure a
    _ -> DesperateSearch <$> liftRunMessage msg attrs
