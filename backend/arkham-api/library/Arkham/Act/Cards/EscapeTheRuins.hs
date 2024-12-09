module Arkham.Act.Cards.EscapeTheRuins (
  EscapeTheRuins (..),
  escapeTheRuins,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.Matcher
import Arkham.Trait

newtype EscapeTheRuins = EscapeTheRuins ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

escapeTheRuins :: ActCard EscapeTheRuins
escapeTheRuins = act (3, A) EscapeTheRuins Cards.escapeTheRuins Nothing

instance HasModifiersFor EscapeTheRuins where
  getModifiersFor (EscapeTheRuins a) = do
    n <- getVengeanceInVictoryDisplay
    modifySelectWhen a (n >= 3) (EnemyWithTrait Serpent) [EnemyEvade 1]

instance HasAbilities EscapeTheRuins where
  getAbilities (EscapeTheRuins x) =
    withBaseAbilities
      x
      [ restrictedAbility x 1 AllUndefeatedInvestigatorsResigned
        $ Objective
        $ ForcedAbility AnyWindow
      | onSide A x
      ]

instance RunMessage EscapeTheRuins where
  runMessage msg a@(EscapeTheRuins attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      resignedWithRelicOfAges <-
        resignedWith
          Assets.relicOfAgesADeviceOfSomeSort
      push $ scenarioResolution $ if resignedWithRelicOfAges then 1 else 3
      pure a
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    _ -> EscapeTheRuins <$> runMessage msg attrs
