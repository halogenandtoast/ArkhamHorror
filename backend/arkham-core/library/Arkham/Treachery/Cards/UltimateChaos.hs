module Arkham.Treachery.Cards.UltimateChaos (
  ultimateChaos,
  UltimateChaos (..),
)
where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype UltimateChaos = UltimateChaos TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ultimateChaos :: TreacheryCard UltimateChaos
ultimateChaos = treachery UltimateChaos Cards.ultimateChaos

instance RunMessage UltimateChaos where
  runMessage msg t@(UltimateChaos attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ revelationSkillTest iid attrs #willpower 4
      pure t
    AfterRevelation iid tid | tid == toId attrs -> do
      instances <- selectList $ treacheryIs Cards.ultimateChaos
      when (length instances >= 3) $ do
        azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
        investigators <- getInvestigators
        player <- getPlayer iid
        pushAll
          $ map (toDiscard attrs) instances
          <> [ chooseOne
                player
                [ Label "Place 1 Doom on Azathoth" [PlaceDoom (toSource attrs) (toTarget azathoth) 1]
                , Label
                    "Azathoth attacks each investigator in player order"
                    [toMessage $ enemyAttack azathoth (toSource attrs) investigator | investigator <- investigators]
                ]
             ]
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
      pushWhen (n >= 3) $ GainSurge (toSource attrs) (toTarget attrs)
      pushWhen (n >= 2) $ assignDamageAndHorror iid attrs 1 1
      push $ AttachTreachery (toId attrs) (toTarget azathoth)
      pure t
    _ -> UltimateChaos <$> runMessage msg attrs
