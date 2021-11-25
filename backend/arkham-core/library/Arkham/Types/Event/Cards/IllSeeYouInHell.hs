module Arkham.Types.Event.Cards.IllSeeYouInHell
  ( illSeeYouInHell
  , IllSeeYouInHell(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Matcher hiding (InvestigatorDefeated)
import Arkham.Types.Message

newtype IllSeeYouInHell = IllSeeYouInHell EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

illSeeYouInHell :: EventCard IllSeeYouInHell
illSeeYouInHell = event IllSeeYouInHell Cards.illSeeYouInHell

instance EventRunner env => RunMessage env IllSeeYouInHell where
  runMessage msg e@(IllSeeYouInHell attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      enemies <- selectList
        (EnemyIsEngagedWith (InvestigatorWithId iid) <> NonEliteEnemy)
      let
        defeatEnemyMessages =
          map (\enemy -> DefeatEnemy enemy iid (toSource attrs)) enemies
      e <$ pushAll
        (defeatEnemyMessages
        <> [ InvestigatorDefeated (toSource attrs) iid
           , SufferTrauma iid 1 0
           , Discard (toTarget attrs)
           ]
        )
    _ -> IllSeeYouInHell <$> runMessage msg attrs
