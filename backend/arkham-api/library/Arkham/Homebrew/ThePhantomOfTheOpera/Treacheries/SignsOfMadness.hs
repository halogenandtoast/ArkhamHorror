module Arkham.Homebrew.ThePhantomOfTheOpera.Treacheries.SignsOfMadness (signsOfMadness) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Treacheries qualified as Cards
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype SignsOfMadness = SignsOfMadness TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

signsOfMadness :: TreacheryCard SignsOfMadness
signsOfMadness = treachery SignsOfMadness Cards.signsOfMadness

instance HasModifiersFor SignsOfMadness where
  getModifiersFor (SignsOfMadness a) = case a.placement of
    AttachedToLocation lid -> modified_ a lid [ShroudModifier 2]
    _ -> pure mempty

instance HasAbilities SignsOfMadness where
  getAbilities (SignsOfMadness a) =
    [ restricted a 1 (OnSameLocation <> exists (EnemyWithPlacement InTheShadows)) (FastAbility Free)
    , restricted a 2 OnSameLocation doubleActionAbility
    ]

instance RunMessage SignsOfMadness where
  runMessage msg t@(SignsOfMadness attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      let without = LocationWithoutTreachery $ treacheryIs Cards.signsOfMadness
      locations <- selectMaxBy LocationConcealedCards length without
      candidates <- if null locations then select without else pure locations
      unless (null candidates) $ chooseOrRunOneM iid $ targets candidates $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid $ makeDecoyAt iid
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> SignsOfMadness <$> liftRunMessage msg attrs
