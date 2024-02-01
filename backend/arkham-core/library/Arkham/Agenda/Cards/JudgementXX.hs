module Arkham.Agenda.Cards.JudgementXX (
  JudgementXX (..),
  judgementXX,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner hiding (InvestigatorDefeated)
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.DefeatedBy
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Investigator.Types (Field (InvestigatorCardCode))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Monster))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype JudgementXX = JudgementXX AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

judgementXX :: AgendaCard JudgementXX
judgementXX = agenda (1, A) JudgementXX Cards.judgementXX (Static 12)

instance HasAbilities JudgementXX where
  getAbilities (JudgementXX a) =
    [ mkAbility a 1 $ ForcedAbility $ PlacedDoomCounter Timing.After AnySource AnyTarget
    , mkAbility a 2
        $ ForcedAbility
        $ InvestigatorDefeated
          Timing.When
          ByAny
          You
    ]

toDefeatedInfo :: [Window] -> Source
toDefeatedInfo [] = error "Invalid call"
toDefeatedInfo ((windowType -> Window.InvestigatorDefeated defeatedBy _) : _) = defeatedBySource defeatedBy
toDefeatedInfo (_ : xs) = toDefeatedInfo xs

instance RunMessage JudgementXX where
  runMessage msg a@(JudgementXX attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> pure a
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      investigators <- getInvestigatorPlayers
      n <- getDoomCount
      let damage = if n >= 5 then 2 else 1
      pushAll
        $ [ chooseOne
            player
            [ Label "Take damage" [assignDamage investigator attrs damage]
            , Label "Take horror" [assignHorror investigator attrs damage]
            ]
          | (investigator, player) <- investigators
          ]
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 (toDefeatedInfo -> source) _ -> do
      push $ RevertAgenda $ toId attrs
      cardCode <- field InvestigatorCardCode iid
      let
        handleOther = do
          n <- getDoomCount
          let
            key =
              if n >= 5 && isSource attrs source
                then DisappearedIntoTheMist
                else WasPulledIntoTheSpectralRealm
          push $ RecordSetInsert key [recorded cardCode]
      case source of
        (EnemyAttackSource eid) -> do
          isTheSpectralWatcher <- eid <=~> enemyIs Enemies.theSpectralWatcher
          isMonster <- eid <=~> EnemyWithTrait Monster
          when isTheSpectralWatcher
            $ push
            $ RecordSetInsert
              WasTakenByTheWatcher
              [recorded cardCode]
          when isMonster
            $ push
            $ RecordSetInsert
              WasClaimedBySpecters
              [recorded cardCode]
          when (not isMonster && not isTheSpectralWatcher) handleOther
        _ -> handleOther
      push $ AdvanceAgenda $ toId attrs
      pure a
    _ -> JudgementXX <$> runMessage msg attrs
