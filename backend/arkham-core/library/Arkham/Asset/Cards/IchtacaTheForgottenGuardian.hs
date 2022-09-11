module Arkham.Asset.Cards.IchtacaTheForgottenGuardian
  ( ichtacaTheForgottenGuardian
  , IchtacaTheForgottenGuardian(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype IchtacaTheForgottenGuardian = IchtacaTheForgottenGuardian AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ichtacaTheForgottenGuardian :: AssetCard IchtacaTheForgottenGuardian
ichtacaTheForgottenGuardian =
  ally IchtacaTheForgottenGuardian Cards.ichtacaTheForgottenGuardian (3, 2)

instance RunMessage IchtacaTheForgottenGuardian where
  runMessage msg (IchtacaTheForgottenGuardian attrs) =
    IchtacaTheForgottenGuardian <$> runMessage msg attrs
