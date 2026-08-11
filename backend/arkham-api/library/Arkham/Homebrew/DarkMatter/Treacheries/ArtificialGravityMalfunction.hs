module Arkham.Homebrew.DarkMatter.Treacheries.ArtificialGravityMalfunction (
  artificialGravityMalfunction,
) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype ArtificialGravityMalfunction = ArtificialGravityMalfunction TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

artificialGravityMalfunction :: TreacheryCard ArtificialGravityMalfunction
artificialGravityMalfunction =
  treachery ArtificialGravityMalfunction Cards.artificialGravityMalfunction

{- | "While you are at attached location, each move action and [action] ability
costs 1 additional action."
-}
instance HasModifiersFor ArtificialGravityMalfunction where
  getModifiersFor (ArtificialGravityMalfunction a) = case a.attached of
    Just (LocationTarget lid) ->
      modifySelect
        a
        (InvestigatorAt $ LocationWithId lid)
        [AdditionalActionCostOf (AnyActionTarget [IsAction #move, IsAction #activate]) 1]
    _ -> pure ()

-- "[reaction] At the end of the round, if you are at attached location: Discard."
instance HasAbilities ArtificialGravityMalfunction where
  getAbilities (ArtificialGravityMalfunction a) =
    [ restricted a 1 (youExist $ at_ $ LocationWithTreachery (TreacheryWithId a.id))
        $ freeReaction
        $ RoundEnds #when
    ]

instance RunMessage ArtificialGravityMalfunction where
  runMessage msg t@(ArtificialGravityMalfunction attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> ArtificialGravityMalfunction <$> liftRunMessage msg attrs
