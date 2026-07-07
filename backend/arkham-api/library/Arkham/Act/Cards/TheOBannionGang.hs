module Arkham.Act.Cards.TheOBannionGang (theOBannionGang) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyClues))
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach)
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.SkillTest.Base (setIsParley)
import Arkham.Trait (Trait (Criminal))

newtype TheOBannionGang = TheOBannionGang ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOBannionGang :: ActCard TheOBannionGang
theOBannionGang = act (2, A) TheOBannionGang Cards.theOBannionGang Nothing

instance HasModifiersFor TheOBannionGang where
  getModifiersFor (TheOBannionGang a) = do
    when (onSide B a) do
      criminalsWithClues <- select $ EnemyWithTrait Criminal <> EnemyWithClues (atLeast 1)
      modifyEach a criminalsWithClues [AddKeyword Keyword.Aloof]

instance HasAbilities TheOBannionGang where
  getAbilities = actAbilities \a ->
    [ restricted (proxied (enemyIs Enemies.naomiOBannion) a) 1 OnSameLocation
        $ parleyAction (UpTo (Fixed 4) $ ResourceCost 1)
    , onlyOnce
        $ mkAbility a 2
        $ Objective
        $ forced
        $ PlacedCounterOnEnemy #when (enemyIs Enemies.naomiOBannion) AnySource #damage (atLeast 1)
    , onlyOnce
        $ restricted a 3 (exists $ enemyIs Enemies.naomiOBannion <> EnemyWithClues (atLeast 3))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage TheOBannionGang where
  runMessage msg a@(TheOBannionGang attrs) = runQueueT $ case msg of
    UseCardAbility iid source@(isProxySource attrs -> True) 1 _ payment -> do
      sid <- getRandom
      chooseBeginSkillTestEdit
        sid
        iid
        source
        attrs
        [#intellect, #agility]
        (Fixed $ max 0 $ 5 - payment.resources)
        setIsParley
      pure a
    PassedThisSkillTestBy iid source@(isProxySource attrs -> True) n -> do
      clues <- fieldMap InvestigatorClues (min n) iid
      removeClues source iid clues
      withMatch (enemyIs Enemies.naomiOBannion) (placeCluesOn source clues)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide A attrs -> True) _ _ -> do
      laBellaLuna <- selectJust $ LocationWithTitle "La Bella Luna"
      createEnemyAt_ Enemies.sadieSheldon laBellaLuna
      createEnemyAt_ Enemies.gangEnforcer laBellaLuna
      lead <- getLead
      findEncounterCard lead attrs $ card_ $ cardIs Enemies.gangSoldier
      doStep 1 msg
      doStep 2 msg
      advanceActDeck attrs
      pure a
    DoStep 1 (AdvanceAct (isSide A attrs -> True) _ _) -> do
      withMatch (enemyIs Enemies.naomiOBannion) \naomiOBannion -> do
        clues <- field EnemyClues naomiOBannion
        when (clues >= 1) do
          withMatch (enemyIs Enemies.sadieSheldon) (placeTokensOn attrs #damage 2)

          whenAny (EnemyAt "La Bella Luna" <> NonEliteEnemy) do
            lead <- getLead
            withI18n $ chooseAmount' lead "cluesToMove" "$clues" 0 (min 2 clues) attrs

      pure a
    ResolveAmounts iid (getChoiceAmount "$clues" -> n) (isTarget attrs -> True) | n > 0 -> do
      enemies <- select $ EnemyAt "La Bella Luna" <> NonEliteEnemy
      withMatch (enemyIs Enemies.naomiOBannion) \naomiOBannion -> do
        repeated n $ chooseOrRunOneM iid $ targets enemies $ moveTokensTo attrs naomiOBannion #clue 1

      pure a
    DoStep 2 (AdvanceAct (isSide A attrs -> True) _ _) -> do
      selectEach EnemyWithAnyClues disengageFromAll
      pure a
    FoundEncounterCard _iid (isTarget attrs -> True) (toCard -> card) -> do
      withMatch (location_ "La Bella Luna") (createEnemyAt_ card)
      pure a
    _ -> TheOBannionGang <$> liftRunMessage msg attrs
