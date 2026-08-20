module Arkham.Enemy.Cards.TheBlobThatAteEverything.MiGoResearcher (miGoResearcher) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.TheBlobThatAteEverything qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Log (remembered)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.ScenarioLogKey (ScenarioLogKey (TheMiGoResearchWasStopped))
import Arkham.Story.CardDefs.TheBlobThatAteEverything qualified as Stories
import Arkham.Trait (Trait (Ooze))

newtype MiGoResearcher = MiGoResearcher EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miGoResearcher :: EnemyCard MiGoResearcher
miGoResearcher = enemy MiGoResearcher Cards.miGoResearcher

instance HasModifiersFor MiGoResearcher where
  getModifiersFor (MiGoResearcher a) =
    whenM (remembered TheMiGoResearchWasStopped) $ modifySelf a [HealthModifier (-2), EnemyEvade (-2)]

unresearchedOoze :: EnemyMatcher
unresearchedOoze = EnemyWithTrait Ooze <> not_ (EnemyWithToken #resource)

researchOozes :: ReverseQueue m => EnemyAttrs -> m ()
researchOozes attrs = do
  oozes <- select $ unresearchedOoze <> EnemyAt (locationWithEnemy attrs.id)
  stories <- select $ storyIs Stories.preventTheirResearch
  for_ oozes \ooze -> do
    placeTokens (attrs.ability 2) ooze #resource 1
    for_ stories \storyId -> placeTokens (attrs.ability 2) storyId #resource 1

instance HasAbilities MiGoResearcher where
  getAbilities (MiGoResearcher a) =
    [ restricted a 1 (thisExists a ReadyEnemy <> exists unresearchedOoze)
        $ forced
        $ PhaseBegins #when #enemy
    , restricted a 2 (exists $ unresearchedOoze <> EnemyAt (locationWithEnemy a.id))
        $ forced
        $ EnemyMoves #after Anywhere (be a)
    ]

instance RunMessage MiGoResearcher where
  runMessage msg e@(MiGoResearcher attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      alreadyThere <- selectAny $ unresearchedOoze <> EnemyAt (locationWithEnemy attrs.id)
      if alreadyThere
        then researchOozes attrs
        else moveToward attrs $ LocationWithEnemy unresearchedOoze
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> researchOozes attrs >> pure e
    _ -> MiGoResearcher <$> liftRunMessage msg attrs
