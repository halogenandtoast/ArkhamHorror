module Arkham.Investigator.Cards.FinnEdwards (finnEdwards, FinnEdwards (..)) where

import Arkham.Action.Additional
import Arkham.Discover
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Projection

newtype FinnEdwards = FinnEdwards InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

finnEdwards :: InvestigatorCard FinnEdwards
finnEdwards =
  investigator FinnEdwards Cards.finnEdwards
    $ Stats {health = 7, sanity = 7, willpower = 1, intellect = 4, combat = 3, agility = 4}

instance HasChaosTokenValue FinnEdwards where
  getChaosTokenValue iid ElderSign (FinnEdwards attrs) | attrs `is` iid = do
    n <- selectCount ExhaustedEnemy
    pure $ ChaosTokenValue ElderSign (PositiveModifier n)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance HasModifiersFor FinnEdwards where
  getModifiersFor (FinnEdwards attrs) =
    modifySelf attrs [GiveAdditionalAction $ AdditionalAction "Finn Edwards" (toSource attrs) #evade]

instance RunMessage FinnEdwards where
  runMessage msg i@(FinnEdwards attrs) = case msg of
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ n | attrs `is` iid && n >= 2 -> do
      when (token.face == ElderSign) $ do
        mlid <- selectOne $ locationWithInvestigator iid
        for_ mlid $ \lid -> do
          canDiscover <- getCanDiscoverClues NotInvestigate (toId attrs) lid
          hasClues <- fieldP LocationClues (> 0) lid
          player <- getPlayer iid
          pushWhen (hasClues && canDiscover)
            $ chooseOne player
            $ [ Label "Discover 1 clue at your location" [Msg.DiscoverClues iid $ discoverAtYourLocation attrs 1]
              , Label "Do not discover a clue" []
              ]
      pure i
    _ -> FinnEdwards <$> runMessage msg attrs
