module Arkham.Location.Cards.Uptown_296
  ( uptown_296
  , Uptown_296(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Uptown_296 = Uptown_296 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uptown_296 :: LocationCard Uptown_296
uptown_296 = location Uptown_296 Cards.uptown_296 3 (Static 0)

instance HasAbilities Uptown_296 where
  getAbilities (Uptown_296 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Uptown_296 where
  runMessage msg (Uptown_296 attrs) =
    Uptown_296 <$> runMessage msg attrs
