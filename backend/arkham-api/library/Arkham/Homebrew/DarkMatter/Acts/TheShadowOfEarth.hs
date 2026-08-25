module Arkham.Homebrew.DarkMatter.Acts.TheShadowOfEarth (theShadowOfEarth) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.Helpers (getScanningDeck)
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Homebrew.DarkMatter.ScenarioDeckKeys (pattern ScanningDeck)
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Trait (Trait (Crew))

newtype TheShadowOfEarth = TheShadowOfEarth ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theShadowOfEarth :: ActCard TheShadowOfEarth
theShadowOfEarth = act (3, A) TheShadowOfEarth Cards.theShadowOfEarth Nothing

instance HasAbilities TheShadowOfEarth where
  getAbilities (TheShadowOfEarth a) =
    [ mkAbility a 1
        $ Objective
        $ forced
        $ EnemyDefeated #after Anyone ByAny (enemyIs Enemies.theEntity)
    , onlyOnce
        $ restricted a 2 (not_ $ exists $ UneliminatedInvestigator <> not_ ResignedInvestigator)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage TheShadowOfEarth where
  runMessage msg a@(TheShadowOfEarth attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R3
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      record YouHaveWitnessedTheManifestedMadness
      selectEach (AssetWithTrait Crew <> AssetControlledBy Anyone) addToVictory_
      (crew, rest) <- partition (`cardMatch` CardWithTrait Crew) <$> getScanningDeck
      unless (null crew) do
        setScenarioDeck ScanningDeck rest
        traverse_ addToVictory_ crew
      push R4
      pure a
    _ -> TheShadowOfEarth <$> liftRunMessage msg attrs
