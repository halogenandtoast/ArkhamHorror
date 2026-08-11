module Arkham.Homebrew.DarkMatter.Treacheries.LostInTranslation (lostInTranslation) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype LostInTranslation = LostInTranslation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInTranslation :: TreacheryCard LostInTranslation
lostInTranslation = treachery LostInTranslation Cards.lostInTranslation

{- | "Revelation - Put Lost in Translation into play in your threat area and seal
a '0' or [elder_sign] token from the chaos bag on it.
[action] [action]: Discard Lost in Translation."
-}
instance HasAbilities LostInTranslation where
  getAbilities (LostInTranslation a) =
    [restricted a 1 (InThreatAreaOf You) $ doubleActionAbilityWithCost mempty]

instance RunMessage LostInTranslation where
  runMessage msg t@(LostInTranslation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      tokens <- select $ chaosToken_ $ oneOf [ChaosTokenFaceIs Zero, ChaosTokenFaceIs ElderSign]
      for_ (take 1 tokens) $ sealChaosToken iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> LostInTranslation <$> liftRunMessage msg attrs
