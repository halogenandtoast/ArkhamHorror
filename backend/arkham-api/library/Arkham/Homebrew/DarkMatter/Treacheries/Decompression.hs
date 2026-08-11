module Arkham.Homebrew.DarkMatter.Treacheries.Decompression (decompression) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern Access)
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype Decompression = Decompression TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decompression :: TreacheryCard Decompression
decompression = treachery Decompression Cards.decompression

{- | "Forced - At the end of the round: Each investigator at attached location
without the EVA Suit story asset takes 3 direct damage. Discard Decompression."
-}
instance HasAbilities Decompression where
  getAbilities (Decompression a) =
    [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage Decompression where
  runMessage msg t@(Decompression attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      nearest <- select $ NearestLocationTo iid (LocationWithTrait Access)
      for_ (take 1 nearest) $ attachTreachery attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      for_ attrs.attached \target -> case target of
        LocationTarget lid -> do
          iids <-
            select
              $ InvestigatorAt (LocationWithId lid)
              <> not_ (HasMatchingAsset $ assetIs Assets.evaSuit)
          for_ iids \iid -> directDamage iid (attrs.ability 1) 3
        _ -> pure ()
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> Decompression <$> liftRunMessage msg attrs
