module Arkham.Treachery.Cards.PossessionTraitorous
  ( possessionTraitorous
  , PossessionTraitorous(..)
  )
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Criteria
import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Id
import Arkham.Investigator.Attrs (Field(..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Treachery.Runner

newtype PossessionTraitorous = PossessionTraitorous TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

possessionTraitorous :: TreacheryCard PossessionTraitorous
possessionTraitorous = treacheryWith PossessionTraitorous Cards.possessionTraitorous (canBeCommittedL .~ True)

-- because we need this ability to check game state, we simply have it use its
-- ability in every window while it is in your hand
instance HasAbilities PossessionTraitorous where
  getAbilities (PossessionTraitorous a) =
    [restrictedAbility a 1 InYourHand $ SilentForcedAbility AnyWindow]

instance RunMessage PossessionTraitorous where
  runMessage msg t@(PossessionTraitorous attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AddTreacheryToHand iid (toId attrs))
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      horror <- field InvestigatorHorror iid
      sanity <- field InvestigatorSanity iid
      when (horror > sanity * 2) $
        push $ InvestigatorKilled source iid
      pure t
    SkillTestCommitCard _ card | toCardId card == unTreacheryId (toId attrs) -> do
      push $ Discard (toTarget attrs)
      pure t
    _ -> PossessionTraitorous <$> runMessage msg attrs
