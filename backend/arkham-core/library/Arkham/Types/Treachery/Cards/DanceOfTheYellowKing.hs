module Arkham.Types.Treachery.Cards.DanceOfTheYellowKing
  ( danceOfTheYellowKing
  , DanceOfTheYellowKing(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype DanceOfTheYellowKing = DanceOfTheYellowKing TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danceOfTheYellowKing :: TreacheryCard DanceOfTheYellowKing
danceOfTheYellowKing =
  treachery DanceOfTheYellowKing Cards.danceOfTheYellowKing

instance TreacheryRunner env => RunMessage env DanceOfTheYellowKing where
  runMessage msg t@(DanceOfTheYellowKing attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      anyLunatics <- selectAny (EnemyWithTrait Lunatic)
      t <$ if anyLunatics
        then push (RevelationSkillTest iid source SkillWillpower 3)
        else push (Surge iid source)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        lunatics <- selectList (NearestEnemy $ EnemyWithTrait Lunatic)
        lid <- getId iid
        t <$ push
          (chooseOrRunOne
            iid
            [ TargetLabel
                (EnemyTarget eid)
                [ MoveUntil lid (EnemyTarget eid)
                , EnemyEngageInvestigator eid iid
                , EnemyWillAttack iid eid DamageAny
                ]
            | eid <- lunatics
            ]
          )
    _ -> DanceOfTheYellowKing <$> runMessage msg attrs
