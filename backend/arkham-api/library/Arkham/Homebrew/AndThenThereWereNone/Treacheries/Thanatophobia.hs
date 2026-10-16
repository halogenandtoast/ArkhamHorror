-- | "__Revelation__ - Put Thanatophobia into play in your threat area."
module Arkham.Homebrew.AndThenThereWereNone.Treacheries.Thanatophobia (thanatophobia) where

import Arkham.Ability
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Matcher
import Arkham.Trait (Trait (Central))
import Arkham.Treachery.Import.Lifted

newtype Thanatophobia = Thanatophobia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thanatophobia :: TreacheryCard Thanatophobia
thanatophobia = treachery Thanatophobia Cards.thanatophobia

{- | "__Forced__ - At the end of the round, if you are not at a Central location,
take 1 horror. Otherwise, discard Thanatophobia."
-}
instance HasAbilities Thanatophobia where
  getAbilities (Thanatophobia a) =
    [ scenarioI18n
        $ withI18nTooltip "thanatophobia.dread"
        $ restricted a 1 (InThreatAreaOf You)
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage Thanatophobia where
  runMessage msg t@(Thanatophobia attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      atCentral <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Central
      if atCentral
        then toDiscard (attrs.ability 1) attrs
        else assignHorror iid (attrs.ability 1) 1
      pure t
    _ -> Thanatophobia <$> liftRunMessage msg attrs
