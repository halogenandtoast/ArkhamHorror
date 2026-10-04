module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.InnsmouthInfluence (innsmouthInfluence) where

import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Trait (Trait (DeepOne))
import Arkham.Treachery.Import.Lifted

newtype InnsmouthInfluence = InnsmouthInfluence TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innsmouthInfluence :: TreacheryCard InnsmouthInfluence
innsmouthInfluence = treachery InnsmouthInfluence Cards.innsmouthInfluence

-- | "You gain the Deep One trait. You have -1 {willpower}."
instance HasModifiersFor InnsmouthInfluence where
  getModifiersFor (InnsmouthInfluence a) =
    maybe
      (pure mempty)
      (\iid -> modified_ a iid [AddTrait DeepOne, SkillModifier #willpower (-1)])
      a.inThreatAreaOf

instance RunMessage InnsmouthInfluence where
  runMessage msg t@(InnsmouthInfluence attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    _ -> InnsmouthInfluence <$> liftRunMessage msg attrs
