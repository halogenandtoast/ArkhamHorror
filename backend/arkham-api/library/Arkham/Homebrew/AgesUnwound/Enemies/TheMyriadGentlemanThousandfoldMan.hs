module Arkham.Homebrew.AgesUnwound.Enemies.TheMyriadGentlemanThousandfoldMan (
  theMyriadGentlemanThousandfoldMan,
) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.Homebrew.AgesUnwound.Traits (pattern Myriad)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.SkillTest.Base
import Arkham.SkillTestResult
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype TheMyriadGentlemanThousandfoldMan = TheMyriadGentlemanThousandfoldMan EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMyriadGentlemanThousandfoldMan :: EnemyCard TheMyriadGentlemanThousandfoldMan
theMyriadGentlemanThousandfoldMan =
  enemy TheMyriadGentlemanThousandfoldMan Cards.theMyriadGentleman_042

{- | "While there are at least 2 other ready [[Myriad]] enemies at his location,
The Myriad Gentleman gets +1 fight."
-}
instance HasModifiersFor TheMyriadGentlemanThousandfoldMan where
  getModifiersFor (TheMyriadGentlemanThousandfoldMan a) = do
    others <-
      selectCount
        $ ReadyEnemy
        <> EnemyWithTrait Myriad
        <> not_ (be a)
        <> EnemyAt (locationWithEnemy a.id)
    modifySelfWhen a (others >= 2) [EnemyFight 1]

instance HasAbilities TheMyriadGentlemanThousandfoldMan where
  getAbilities (TheMyriadGentlemanThousandfoldMan a) =
    extend
      a
      [ -- "When The Myriad Gentleman is defeated, any excess damage may be dealt
        -- to other copies of this enemy at its location."
        restricted a 1 (exists $ otherCopiesAt a)
          $ freeReaction
          $ EnemyDealtExcessDamage #when AnyDamageEffect (be a) AnySource
      , -- "[reaction] After you successfully evade The Myriad Gentleman: For each
        -- point you succeeded by, you may evade a copy of this enemy engaged with
        -- you."
        restricted a 2 (exists $ ReadyEnemy <> copiesOf a <> EnemyIsEngagedWith You)
          $ freeReaction
          $ SkillTestResult #after You (whileEvading a) (SuccessResult $ atLeast 1)
      ]

copiesOf :: EnemyAttrs -> EnemyMatcher
copiesOf a = enemyIs Cards.theMyriadGentleman_042 <> not_ (be a)

otherCopiesAt :: EnemyAttrs -> EnemyMatcher
otherCopiesAt a = copiesOf a <> EnemyAt (locationWithEnemy a.id)

toExcessDamage :: [Window] -> Int
toExcessDamage [] = 0
toExcessDamage ((windowType -> Window.DealtExcessDamage _ _ _ n) : _) = n
toExcessDamage (_ : xs) = toExcessDamage xs

instance RunMessage TheMyriadGentlemanThousandfoldMan where
  runMessage msg e@(TheMyriadGentlemanThousandfoldMan attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (toExcessDamage -> excess) _ | excess > 0 -> do
      copies <- select $ otherCopiesAt attrs
      unless (null copies) do
        chooseEnemyAmounts iid (scenarioI18n $ ikey' "label.dealExcessDamage") excess copies attrs
      pure e
    ResolveAmounts _ choices (isTarget attrs -> True) -> do
      let assignments = [(EnemyId nu.nuUUID, n) | (nu, n) <- choices, n > 0]
      for_ assignments \(eid, n) ->
        nonAttackEnemyDamage Nothing (attrs.ability 1) n eid
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      mSkillTest <- getSkillTest
      case skillTestResult <$> mSkillTest of
        Just (SucceededBy _ n) | n > 0 -> do
          copies <- select $ ReadyEnemy <> copiesOf attrs <> enemyEngagedWith iid
          unless (null copies) do
            chooseNM iid (min n (length copies)) $ targets copies $ automaticallyEvadeEnemy iid
        _ -> pure ()
      pure e
    _ -> TheMyriadGentlemanThousandfoldMan <$> liftRunMessage msg attrs
