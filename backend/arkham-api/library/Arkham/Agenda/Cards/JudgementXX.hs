module Arkham.Agenda.Cards.JudgementXX (judgementXX) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (InvestigatorDefeated)
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.DefeatedBy
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Doom (getDoomCount)
import Arkham.Investigator.Types (Field (InvestigatorCardCode))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Trait (Trait (Monster))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype JudgementXX = JudgementXX AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

judgementXX :: AgendaCard JudgementXX
judgementXX = agenda (1, A) JudgementXX Cards.judgementXX (Static 12)

instance HasAbilities JudgementXX where
  getAbilities (JudgementXX a) =
    [ restricted a 1 NotSetup $ forced $ PlacedDoomCounter #after AnySource AnyTarget
    , mkAbility a 2 $ forced $ InvestigatorDefeated #when ByAny You
    ]

toDefeatedInfo :: [Window] -> Source
toDefeatedInfo [] = error "Invalid call"
toDefeatedInfo ((windowType -> Window.InvestigatorDefeated defeatedBy _) : _) = defeatedBySource defeatedBy
toDefeatedInfo (_ : xs) = toDefeatedInfo xs

instance RunMessage JudgementXX where
  runMessage msg a@(JudgementXX attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> pure a
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      n <- getDoomCount
      let damage = if n >= 5 then 2 else 1
      eachInvestigator \iid -> do
        chooseOneM iid do
          labeled "Take damage" $ assignDamage iid attrs damage
          labeled "Take horror" $ assignHorror iid attrs damage
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 (toDefeatedInfo -> source) _ -> do
      push $ AdvanceAgenda attrs.id
      cardCode <- field InvestigatorCardCode iid
      let
        handleOther = do
          n <- getDoomCount
          let
            key =
              if n >= 5 && isSource attrs source
                then DisappearedIntoTheMist
                else WasPulledIntoTheSpectralRealm
          recordSetInsert key [cardCode]
      case source of
        (EnemyAttackSource eid) -> do
          isTheSpectralWatcher <- eid <=~> enemyIs Enemies.theSpectralWatcher
          when isTheSpectralWatcher do
            recordSetInsert WasTakenByTheWatcher [cardCode]
          isMonster <- eid <=~> EnemyWithTrait Monster
          when isMonster do
            recordSetInsert WasClaimedBySpecters [cardCode]
          when (not isMonster && not isTheSpectralWatcher) handleOther
        _ -> handleOther
      push $ RevertAgenda attrs.id
      pure a
    _ -> JudgementXX <$> liftRunMessage msg attrs
