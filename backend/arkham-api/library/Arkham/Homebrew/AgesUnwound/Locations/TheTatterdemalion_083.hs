module Arkham.Homebrew.AgesUnwound.Locations.TheTatterdemalion_083 (theTatterdemalion_083) where

import Arkham.Ability
import Arkham.Helpers.Window (evadedEnemy)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TheTatterdemalion_083 = TheTatterdemalion_083 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /The Tatterdemalion/ -- a starship in 2147.
theTatterdemalion_083 :: LocationCard TheTatterdemalion_083
theTatterdemalion_083 =
  locationWith TheTatterdemalion_083 Cards.theTatterdemalion_083 3 (PerPlayer 1)
    $ connectsToL
    .~ ringConnections

{- | "[reaction] After you successfully evade an enemy at this location: Defeat
that enemy. (Group limit once per game.)" / "Forced - At the end of your turn:
Test [willpower] (3). If you fail, you must either discard an asset you control,
or take 1 horror for each point you failed by."

The reaction takes index 2 so that 'endOfTurnAbility' keeps index 1 on every
Adrift location.
-}
instance HasAbilities TheTatterdemalion_083 where
  getAbilities (TheTatterdemalion_083 a) =
    extendRevealed
      a
      [ restricted a endOfTurnAbility Here $ forced $ TurnEnds #when You
      , groupLimit PerGame
          $ restricted a 2 Here
          $ freeReaction (EnemyEvaded #after You $ enemyAt a.id)
      ]

instance RunMessage TheTatterdemalion_083 where
  runMessage msg l@(TheTatterdemalion_083 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 3)
      pure l
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      canDiscardAsset <- selectAny $ assetControlledBy iid <> DiscardableAsset
      chooseOrRunOneM iid $ withI18n do
        countVar 1
          $ labeledValidate canDiscardAsset "discardAssets"
          $ chooseAndDiscardAssetMatching iid (attrs.ability 1) (assetControlledBy iid <> DiscardableAsset)
        countVar n $ labeled "takeHorror" $ assignHorror iid (attrs.ability 1) n
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 (evadedEnemy -> eid) _ -> do
      push $ DefeatEnemy eid iid (attrs.ability 2)
      pure l
    _ -> TheTatterdemalion_083 <$> liftRunMessage msg attrs
