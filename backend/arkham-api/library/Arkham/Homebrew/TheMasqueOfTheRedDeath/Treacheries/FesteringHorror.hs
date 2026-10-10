module Arkham.Homebrew.TheMasqueOfTheRedDeath.Treacheries.FesteringHorror (festeringHorror) where

import Arkham.Ability
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Window.Clue (discoveredClues)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted hiding (DiscoverClues)

newtype FesteringHorror = FesteringHorror TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

festeringHorror :: TreacheryCard FesteringHorror
festeringHorror = treachery FesteringHorror Cards.festeringHorror

instance HasAbilities FesteringHorror where
  getAbilities (FesteringHorror a) =
    -- "After an investigator discovers 1 or more clues" is tied to the discoverer
    -- with `You`: a seat-agnostic `Who` offers the Forced trigger to every seat and
    -- the resources would be placed once per investigator (#5685).
    [ mkAbility a 1 $ forced $ DiscoverClues #after You Anywhere (atLeast 1)
    , groupLimit PerRound $ mkAbility a 2 $ forced $ RoundEnds #when
    ]

instance RunMessage FesteringHorror where
  runMessage msg t@(FesteringHorror attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 (discoveredClues -> n) _ -> do
      placeTokens (attrs.ability 1) attrs #resource n
      pure t
    -- "For every 1[per_investigator] resources" is a rate, not a threshold: three
    -- resources at two players is one asset each, five is two.
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      perInvestigator <- perPlayer 1
      let n = attrs.resources `div` perInvestigator
      when (n > 0) $ eachInvestigator \iid ->
        replicateM_ n $ chooseAndDiscardAsset iid (attrs.ability 2)
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> FesteringHorror <$> liftRunMessage msg attrs
