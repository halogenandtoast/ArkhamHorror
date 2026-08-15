module Arkham.Homebrew.DarkMatter.Acts.PublicSchool187V10 (publicSchool187V10) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Matcher qualified as Matcher

newtype PublicSchool187V10 = PublicSchool187V10 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

publicSchool187V10 :: ActCard PublicSchool187V10
publicSchool187V10 = act (1, A) PublicSchool187V10 Cards.publicSchool187V10 Nothing

instance HasAbilities PublicSchool187V10 where
  getAbilities (PublicSchool187V10 a) =
    [ mkAbility a 1
        $ Objective
        $ forced
        $ Matcher.RevealLocation #after Anyone (locationIs Locations.entranceHall)
    ]

instance RunMessage PublicSchool187V10 where
  runMessage msg a@(PublicSchool187V10 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- top row
      placeLocationInGrid_ (Pos (-1) 2) =<< fetchCard Locations.library
      placeLocationInGrid_ (Pos 0 2) =<< fetchCard Locations.biologyLab
      placeLocationInGrid_ (Pos 1 2) =<< fetchCard Locations.gymnasium
      -- middle outside locations
      placeLocationInGrid_ (Pos (-1) 1) =<< fetchCard Locations.cafeteria
      placeLocationInGrid_ (Pos 1 1) =<< fetchCard Locations.classroomK2
      advanceActDeck attrs
      pure a
    _ -> PublicSchool187V10 <$> liftRunMessage msg attrs
