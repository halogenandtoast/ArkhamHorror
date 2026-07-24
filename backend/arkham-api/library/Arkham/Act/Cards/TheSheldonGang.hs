module Arkham.Act.Cards.TheSheldonGang (theSheldonGang) where

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

newtype TheSheldonGang = TheSheldonGang ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSheldonGang :: ActCard TheSheldonGang
theSheldonGang = act (2, A) TheSheldonGang Cards.theSheldonGang Nothing

instance HasModifiersFor TheSheldonGang where
  getModifiersFor (TheSheldonGang a) = do
    when (onSide B a) do
      criminalsWithClues <- select $ EnemyWithTrait Criminal <> EnemyWithClues (atLeast 1)
      modifyEach a criminalsWithClues [AddKeyword Keyword.Aloof]

instance HasAbilities TheSheldonGang where
  getAbilities = actAbilities \a ->
    [ restricted (proxied (enemyIs Enemies.sadieSheldon) a) 1 OnSameLocation
        $ parleyAction (UpTo (Fixed 4) $ ResourceCost 1)
    , onlyOnce
        $ mkAbility a 2
        $ Objective
        $ forced
        $ PlacedCounterOnEnemy #when (enemyIs Enemies.sadieSheldon) AnySource #damage (atLeast 1)
    , onlyOnce
        $ restricted a 3 (exists $ enemyIs Enemies.sadieSheldon <> EnemyWithClues (atLeast 3))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage TheSheldonGang where
  runMessage msg a@(TheSheldonGang attrs) = runQueueT $ case msg of
    UseCardAbility iid source@(isProxySource attrs -> True) 1 _ payment -> do
      sid <- getRandom
      chooseBeginSkillTestEdit
        sid
        iid
        source
        attrs
        [#willpower, #combat]
        (Fixed $ max 0 $ 5 - payment.resources)
        setIsParley
      pure a
    PassedThisSkillTestBy iid source@(isProxySource attrs -> True) n -> do
      clues <- fieldMap InvestigatorClues (min n) iid
      removeClues source iid clues
      withMatch (enemyIs Enemies.sadieSheldon) (placeCluesOn source clues)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      hibbs <- selectJust $ LocationWithTitle "Hibb's Roadhouse"
      createEnemyAt_ Enemies.naomiOBannion hibbs
      createEnemyAt_ Enemies.gangEnforcer hibbs
      lead <- getLead
      findEncounterCard lead attrs $ card_ $ cardIs Enemies.gangSoldier
      doStep 1 msg
      doStep 2 msg
      advanceActDeck attrs
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      withMatch (enemyIs Enemies.sadieSheldon) \sadieSheldon -> do
        clues <- field EnemyClues sadieSheldon
        when (clues >= 1) do
          withMatch (enemyIs Enemies.naomiOBannion) (placeTokensOn attrs #damage 2)

          whenAny (EnemyAt "Hibb's Roadhouse" <> NonEliteEnemy) do
            lead <- getLead
            withI18n $ chooseAmount' lead "cluesToMove" "$clues" 0 (min 2 clues) attrs

      pure a
    ResolveAmounts iid (getChoiceAmount "$clues" -> n) (isTarget attrs -> True) | n > 0 -> do
      enemies <- select $ EnemyAt "Hibb's Roadhouse" <> NonEliteEnemy
      withMatch (enemyIs Enemies.sadieSheldon) \sadieSheldon -> do
        repeated n $ chooseOrRunOneM iid $ targets enemies $ moveTokensTo attrs sadieSheldon #clue 1

      pure a
    DoStep 2 (AdvanceAct (isSide A attrs -> True) _ _) -> do
      selectEach EnemyWithAnyClues disengageFromAll
      pure a
    FoundEncounterCard _iid (isTarget attrs -> True) (toCard -> card) -> do
      createEnemyAt_ card =<< selectJust (LocationWithTitle "Hibb's Roadhouse")
      pure a
    _ -> TheSheldonGang <$> liftRunMessage msg attrs
