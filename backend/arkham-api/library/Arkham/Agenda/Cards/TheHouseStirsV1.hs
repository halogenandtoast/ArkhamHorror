module Arkham.Agenda.Cards.TheHouseStirsV1 (theHouseStirsV1) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Scenarios.HemlockHouse.Helpers (locationSealCount)
import Arkham.Story.Cards qualified as Stories
import Arkham.Token (Token (..))

newtype TheHouseStirsV1 = TheHouseStirsV1 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHouseStirsV1 :: AgendaCard TheHouseStirsV1
theHouseStirsV1 = agenda (2, A) TheHouseStirsV1 Cards.theHouseStirsV1 (Static 5)

instance HasAbilities TheHouseStirsV1 where
  getAbilities (TheHouseStirsV1 a) =
    [ -- "Forced - When the mythos phase ends: Make a predation test."
      mkAbility a 1
        $ forced
        $ PhaseEnds #when #mythos
    , -- "[fast] place 1 [per_investigator] of your clues on your location:
      -- Ready it and flip it over." (Half of the OR; the seal variant is idx 3.)
      restricted a 2 (exists $ YourLocation)
        $ FastAbility (SameLocationGroupClueCost (PerPlayer 1) YourLocation)
    , -- "[fast] remove a seal from your location: Ready it and flip it over."
      restricted a 3 (exists $ YourLocation <> LocationWithToken Resource)
        $ FastAbility Free
    ]

instance RunMessage TheHouseStirsV1 where
  runMessage msg a@(TheHouseStirsV1 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      thePredatoryHouse <- selectJust $ storyIs Stories.thePredatoryHouse
      sendMessage' thePredatoryHouse $ requestChaosTokens lead (attrs.ability 1) 1
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      getMaybeLocation iid >>= traverse_ \lid -> do
        n <- perPlayer 1
        placeTokens (attrs.ability 2) lid Clue n
      advanceAgendaDeck attrs
      pure a
    UseCardAbility iid (isSource attrs -> True) 3 _ _ -> do
      getMaybeLocation iid >>= traverse_ \lid -> do
        seals <- locationSealCount lid
        when (seals > 0) $ removeTokens (attrs.ability 3) lid Resource 1
      advanceAgendaDeck attrs
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheHouseStirsV1 <$> liftRunMessage msg attrs
