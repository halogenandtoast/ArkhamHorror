module Arkham.Treachery.Cards.ItsGotMe (itsGotMe) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Projection
import Arkham.Token qualified as Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ItsGotMe = ItsGotMe TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

itsGotMe :: TreacheryCard ItsGotMe
itsGotMe = treachery ItsGotMe Cards.itsGotMe

instance HasAbilities ItsGotMe where
  getAbilities (ItsGotMe a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ RoundEnds #when
    , restricted
        a
        2
        ( InThreatAreaOf You
            <> youExist (at_ $ locationIs Locations.researchSiteTheBlobThatAteEverything)
        )
        $ actionAbilityWithCost (SpendTokenCost Token.Resource (TargetIs ScenarioTarget))
    ]

instance RunMessage ItsGotMe where
  runMessage msg t@(ItsGotMe attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs #resource 1
      current <- field TreacheryResources attrs.id
      when (current + 1 >= 6) $ for_ attrs.inThreatAreaOf \iid -> kill (attrs.ability 1) iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> ItsGotMe <$> liftRunMessage msg attrs
