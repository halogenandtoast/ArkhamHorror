module Arkham.Treachery.Cards.SpectralMist (
  SpectralMist (..),
  spectralMist,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Investigator
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillType ()
import Arkham.Source
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype SpectralMist = SpectralMist TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

spectralMist :: TreacheryCard SpectralMist
spectralMist = treachery SpectralMist Cards.spectralMist

instance HasModifiersFor SpectralMist where
  getModifiersFor _ (SpectralMist a) = do
    mInvestigator <- getSkillTestInvestigator
    case mInvestigator of
      Just iid -> do
        lid <- getJustLocation iid
        pure $ toModifiers a [Difficulty 1 | treacheryOnLocation lid a]
      _ -> pure []

instance HasAbilities SpectralMist where
  getAbilities (SpectralMist a) =
    [restrictedAbility a 1 OnSameLocation $ ActionAbility [] $ ActionCost 1]

instance RunMessage SpectralMist where
  runMessage msg t@(SpectralMist attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      targets <-
        selectTargets
          $ LocationWithTrait Bayou
          <> NotLocation
            (LocationWithTreachery $ treacheryIs Cards.spectralMist)
      player <- getPlayer iid
      pushWhen (notNull targets)
        $ chooseOne
          player
          [ targetLabel target [AttachTreachery treacheryId target]
          | target <- targets
          ]
      SpectralMist <$> runMessage msg attrs
    UseCardAbility iid (TreacherySource tid) 1 _ _ | tid == treacheryId -> do
      push $ beginSkillTest iid attrs attrs #intellect 2
      pure t
    PassedSkillTest iid _ source SkillTestInitiatorTarget {} _ _ | isSource attrs source -> do
      t <$ push (toDiscardBy iid (toAbilitySource attrs 1) attrs)
    _ -> SpectralMist <$> runMessage msg attrs
