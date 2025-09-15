#!/usr/bin/env python3
"""
Benchmark statistics.
"""

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns
from pathlib import Path
import sys

def load_and_validate_data(filepath):
    """Load CSV"""
    try:
        if not Path(filepath).exists():
            raise FileNotFoundError(f"File '{filepath}' not found")
        
        df = pd.read_csv(filepath)
        
        # Validate required columns
        required_cols = ['benchmark', 'run', 'result']
        missing_cols = [col for col in required_cols if col not in df.columns]
        if missing_cols:
            raise ValueError(f"Missing required columns: {missing_cols}")
        
        # Check for missing values
        if df.isnull().any().any():
            print("Warning: Found missing values in data")
            df = df.dropna()
        
        # Validate numeric results
        if not pd.api.types.is_numeric_dtype(df['result']):
            df['result'] = pd.to_numeric(df['result'], errors='coerce')
            df = df.dropna(subset=['result'])
        
        return df
        
    except Exception as e:
        print(f"Error loading data: {e}")
        sys.exit(1)

def compute_comprehensive_stats(pivot):
    """Compute comprehensive benchmark statistics."""
    speedup = pivot['speedup']
    
    stats = {
        'total': len(pivot),
        'improved': (speedup > 1).sum(),
        'worsened': (speedup < 1).sum(),
        'unchanged': (speedup == 1).sum(),
        'mean_speedup': speedup.mean(),
        'median_speedup': speedup.median(),
        'std_speedup': speedup.std(),
        'max_speedup': speedup.max(),
        'min_speedup': speedup.min(),
        'q25': speedup.quantile(0.25),
        'q75': speedup.quantile(0.75),
        'geometric_mean': np.exp(np.log(speedup).mean()),
        'best_benchmark': speedup.idxmax(),
        'worst_benchmark': speedup.idxmin()
    }
    
    # Performance categories
    stats['significant_improvement'] = (speedup > 1.1).sum()  # >10% improvement
    stats['significant_regression'] = (speedup < 0.9).sum()   # >10% regression
    
    return stats

def print_statistics(stats):
    """Print formatted statistics report."""
    print("=" * 50)
    print("           BENCHMARK STATISTICS REPORT")
    print("=" * 50)
    
    print(f"\n📊 OVERALL SUMMARY:")
    print(f"   Total benchmarks: {stats['total']}")
    print(f"   Improved: {stats['improved']} ({stats['improved']/stats['total']*100:.1f}%)")
    print(f"   Worsened: {stats['worsened']} ({stats['worsened']/stats['total']*100:.1f}%)")
    print(f"   Unchanged: {stats['unchanged']} ({stats['unchanged']/stats['total']*100:.1f}%)")
    
    print(f"\n🚀 PERFORMANCE METRICS:")
    print(f"   Mean speedup: {stats['mean_speedup']:.3f}x")
    print(f"   Median speedup: {stats['median_speedup']:.3f}x")
    print(f"   Geometric mean: {stats['geometric_mean']:.3f}x")
    print(f"   Standard deviation: {stats['std_speedup']:.3f}")
    
    print(f"\n📈 EXTREMES:")
    print(f"   Best speedup: {stats['max_speedup']:.3f}x ({stats['best_benchmark']})")
    print(f"   Worst slowdown: {stats['min_speedup']:.3f}x ({stats['worst_benchmark']})")
    
    print(f"\n🎯 SIGNIFICANT CHANGES (>10%):")
    print(f"   Significant improvements: {stats['significant_improvement']}")
    print(f"   Significant regressions: {stats['significant_regression']}")
    
    print(f"\n📊 QUARTILES:")
    print(f"   25th percentile: {stats['q25']:.3f}x")
    print(f"   75th percentile: {stats['q75']:.3f}x")

def create_visualizations(pivot, stats):
    """Create comprehensive visualization dashboard."""
    # Set up the plotting style
    plt.style.use('seaborn-v0_8')
    fig = plt.figure(figsize=(16, 12))
    
    # 1. Main histogram
    ax1 = plt.subplot(2, 3, (1, 2))
    n, bins, patches = plt.hist(pivot['speedup'], bins=30, alpha=0.7, 
                               edgecolor='black', linewidth=0.5)
    
    # Color bars based on performance
    for i, (patch, bin_val) in enumerate(zip(patches, bins[:-1])):
        if bin_val > 1.1:
            patch.set_facecolor('green')
            patch.set_alpha(0.7)
        elif bin_val < 0.9:
            patch.set_facecolor('red')
            patch.set_alpha(0.7)
        else:
            patch.set_facecolor('gray')
            patch.set_alpha(0.5)
    
    plt.axvline(1, color='black', linestyle='--', linewidth=2, label='No change (1.0x)')
    plt.axvline(stats['mean_speedup'], color='blue', linestyle='-', 
                linewidth=2, label=f'Mean ({stats["mean_speedup"]:.2f}x)')
    plt.axvline(stats['median_speedup'], color='orange', linestyle='-', 
                linewidth=2, label=f'Median ({stats["median_speedup"]:.2f}x)')
    
    plt.xlabel('Speedup (baseline / myopt)', fontsize=12)
    plt.ylabel('Number of benchmarks', fontsize=12)
    plt.title('Distribution of Benchmark Speedups', fontsize=14, fontweight='bold')
    plt.legend()
    plt.grid(True, alpha=0.3)
    
    # 2. Box plot
    ax2 = plt.subplot(2, 3, 3)
    box_plot = plt.boxplot(pivot['speedup'], patch_artist=True)
    box_plot['boxes'][0].set_facecolor('lightblue')
    plt.ylabel('Speedup', fontsize=12)
    plt.title('Speedup Distribution\n(Box Plot)', fontsize=12, fontweight='bold')
    plt.grid(True, alpha=0.3)
    
    # 3. Top performers
    ax3 = plt.subplot(2, 3, 4)
    top_n = min(10, len(pivot))
    best = pivot.nlargest(top_n, 'speedup')
    colors = plt.cm.Greens(np.linspace(0.4, 0.9, len(best)))
    bars = plt.barh(range(len(best)), best['speedup'], color=colors)
    plt.yticks(range(len(best)), [name[:20] + '...' if len(name) > 20 else name 
                                  for name in best.index])
    plt.xlabel('Speedup', fontsize=12)
    plt.title(f'Top {len(best)} Performers', fontsize=12, fontweight='bold')
    plt.axvline(1, color='red', linestyle='--', alpha=0.7)
    
    # Add value labels on bars
    for i, (bar, val) in enumerate(zip(bars, best['speedup'])):
        plt.text(val + 0.01, i, f'{val:.2f}x', va='center', fontsize=9)
    
    # 4. Bottom performers
    ax4 = plt.subplot(2, 3, 5)
    bottom_n = min(10, len(pivot))
    worst = pivot.nsmallest(bottom_n, 'speedup')
    colors = plt.cm.Reds(np.linspace(0.4, 0.9, len(worst)))
    bars = plt.barh(range(len(worst)), worst['speedup'], color=colors)
    plt.yticks(range(len(worst)), [name[:20] + '...' if len(name) > 20 else name 
                                   for name in worst.index])
    plt.xlabel('Speedup', fontsize=12)
    plt.title(f'Bottom {len(worst)} Performers', fontsize=12, fontweight='bold')
    plt.axvline(1, color='red', linestyle='--', alpha=0.7)
    
    # Add value labels on bars
    for i, (bar, val) in enumerate(zip(bars, worst['speedup'])):
        plt.text(val + 0.01, i, f'{val:.2f}x', va='center', fontsize=9)
    
    # 5. Performance categories pie chart
    ax5 = plt.subplot(2, 3, 6)
    categories = ['Improved', 'Worsened', 'Unchanged']
    sizes = [stats['improved'], stats['worsened'], stats['unchanged']]
    colors = ['lightgreen', 'lightcoral', 'lightgray']
    
    wedges, texts, autotexts = plt.pie(sizes, labels=categories, colors=colors, 
                                       autopct='%1.1f%%', startangle=90)
    plt.title('Performance Categories', fontsize=12, fontweight='bold')
    
    plt.tight_layout()
    return fig

def main():
    """Main function to run the benchmark analysis."""
    filename = "results-dyn.csv"
    
    print(f"Loading benchmark data from '{filename}'...")
    df = load_and_validate_data(filename)
    print(f"Successfully loaded {len(df)} data points")
    
    # Create pivot table
    pivot = df.pivot(index='benchmark', columns='run', values='result')
    
    # Validate that we have both baseline and myopt columns
    if 'baseline' not in pivot.columns or 'myopt' not in pivot.columns:
        available_runs = list(pivot.columns)
        raise ValueError(f"Expected 'baseline' and 'myopt' runs, found: {available_runs}")
    
    # Calculate speedup
    pivot['speedup'] = pivot['baseline'] / pivot['myopt']
    
    # Remove any invalid speedups (inf, nan)
    pivot = pivot[np.isfinite(pivot['speedup'])]
    
    if len(pivot) == 0:
        raise ValueError("No valid speedup calculations could be made")
    
    # Compute statistics
    stats = compute_comprehensive_stats(pivot)
    
    # Print results
    print_statistics(stats)
    
    # Create visualizations
    print("\nGenerating visualizations...")
    fig = create_visualizations(pivot, stats)
    
    # Save the plot
    output_file = "benchmark_analysis.png"
    fig.savefig(output_file, dpi=300, bbox_inches='tight')
    print(f"Visualization saved as '{output_file}'")
    
    # Show the plot
    plt.show()
    
    # Optional: Save detailed results to CSV
    detailed_results = pivot.copy()
    detailed_results = detailed_results.sort_values('speedup', ascending=False)
    detailed_results.to_csv('detailed_benchmark_results.csv')
    print("Detailed results saved to 'detailed_benchmark_results.csv'")

if __name__ == "__main__":
    main()
