module Arkham.Story.Cards.TheUnsealing (theUnsealing) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as ScarletKeys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.Window (getEnemy)
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted hiding (EnemyAttacks, EnemyEvaded)

newtype TheUnsealing = TheUnsealing StoryAttrs
  deriving anyclass IsStory
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theUnsealing :: StoryCard TheUnsealing
theUnsealing = story TheUnsealing Cards.theUnsealing

instance HasModifiersFor TheUnsealing where
  getModifiersFor (TheUnsealing a) = do
    mAntiprismEnemy <- selectOne $ EnemyWithScarletKey $ scarletKeyIs ScarletKeys.theTwistedAntiprism
    enemies <- select NonWeaknessEnemy
    for_ enemies \enemy -> do
      if Just enemy == mAntiprismEnemy
        then modified_ a enemy [AddKeyword $ Patrol "Galata Docks"]
        else
          modified_
            a
            enemy
            [ AddKeyword Hunter
            , ForcePrey (Prey $ InvestigatorWithScarletKey (scarletKeyIs ScarletKeys.theTwistedAntiprism))
            ]

instance HasAbilities TheUnsealing where
  getAbilities (TheUnsealing a) =
    [ mkAbility a 1 $ forced (WouldPlaceDoomCounter #when AnySource #enemy)
    , mkAbility a 2
        $ forced
        $ EnemyAttacks
          #after
          (InvestigatorWithScarletKey (scarletKeyIs ScarletKeys.theTwistedAntiprism))
          AnyEnemyAttack
          NonWeaknessEnemy
    , mkAbility a 3
        $ freeReaction
        $ EnemyEvaded #after You (EnemyWithScarletKey (scarletKeyIs ScarletKeys.theTwistedAntiprism))
    , mkAbility a 3
        $ ForcedWhen NoRestriction
        $ freeReaction
        $ IfEnemyDefeated #after You ByAny
        $ EnemyWithScarletKey (scarletKeyIs ScarletKeys.theTwistedAntiprism)
    ]

instance RunMessage TheUnsealing where
  runMessage msg s@(TheUnsealing attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    UseCardAbility _ (isSource attrs -> True) 1 ws@(getEnemy -> x) _ -> do
      cancelWindowBatch ws
      push $ HunterMove x
      temporaryModifier x attrs DoNotExhaust do
        push $ ForTarget (toTarget x) EnemiesAttack
      pure s
    UseCardAbility _iid (isSource attrs -> True) 2 (getEnemy -> x) _ -> do
      twistedAntiprism <- selectJust $ scarletKeyIs ScarletKeys.theTwistedAntiprism
      push $ PlaceScarletKey twistedAntiprism (AttachedToEnemy x)
      pure s
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      twistedAntiprism <- selectJust $ scarletKeyIs ScarletKeys.theTwistedAntiprism
      push $ PlaceScarletKey twistedAntiprism (AttachedToInvestigator iid)
      pure s
    _ -> TheUnsealing <$> liftRunMessage msg attrs
