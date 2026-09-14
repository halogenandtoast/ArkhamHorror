module Arkham.Homebrew.DarkMatter.Acts.InLostCarcosa (inLostCarcosa) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Enemy.CardDefs.ThePathToCarcosa.InhabitantsOfCarcosa qualified as InhabitantsOfCarcosa
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Matcher
import Arkham.Trait (Trait (Cave))

newtype InLostCarcosa = InLostCarcosa ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inLostCarcosa :: ActCard InLostCarcosa
inLostCarcosa = act (1, A) InLostCarcosa Cards.inLostCarcosa Nothing

instance HasAbilities InLostCarcosa where
  getAbilities (InLostCarcosa a) =
    [ onlyOnce
        $ restricted
          a
          1
          (exists $ locationIs Locations.abandonedLander <> RevealedLocation <> LocationWithoutClues)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage InLostCarcosa where
  runMessage msg a@(InLostCarcosa attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      watchedHeir <- getHasRecord YouHaveWatchedThePerformanceOfHeirToCarcosa
      when watchedHeir $ eachInvestigator \iid -> gainClues iid attrs 1
      placeSetAsideLocationsMatching_ (CardWithTrait Cave)
      unless watchedHeir
        $ createSetAsideEnemy_ InhabitantsOfCarcosa.beastOfAldebaran Locations.cyclopeanCaverns
      advanceActDeck attrs
      pure a
    _ -> InLostCarcosa <$> liftRunMessage msg attrs
