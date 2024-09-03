module Arkham.Event.Cards.IllPayYouBack (illPayYouBack, illPayYouBackEffect, IllPayYouBack (..)) where

import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Token

newtype IllPayYouBack = IllPayYouBack EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

illPayYouBack :: EventCard IllPayYouBack
illPayYouBack = event IllPayYouBack Cards.illPayYouBack

instance RunMessage IllPayYouBack where
  runMessage msg e@(IllPayYouBack attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ affectsOthers $ colocatedWith iid <> not_ (InvestigatorWithId iid)
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (InvestigatorTarget iid') -> do
      resources <- field InvestigatorResources iid'
      moveTokens attrs iid' iid Resource resources
      createCardEffect Cards.illPayYouBack (effectMetaTarget iid') attrs iid
      pure e
    _ -> IllPayYouBack <$> liftRunMessage msg attrs

newtype IllPayYouBackEffect = IllPayYouBackEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

illPayYouBackEffect :: EffectArgs -> IllPayYouBackEffect
illPayYouBackEffect = cardEffect IllPayYouBackEffect Cards.illPayYouBack

instance RunMessage IllPayYouBackEffect where
  runMessage msg e@(IllPayYouBackEffect attrs) = runQueueT $ case msg of
    EndRound -> do
      for_ attrs.metaTarget \case
        InvestigatorTarget iid' -> case attrs.target of
          InvestigatorTarget iid -> do
            resources <- field InvestigatorResources iid
            moveTokens attrs iid iid' Resource resources
          _ -> error "Invalid Target"
        _ -> error "Invalid Target"
      disableReturn e
    _ -> IllPayYouBackEffect <$> liftRunMessage msg attrs
